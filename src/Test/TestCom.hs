{-# LANGUAGE TemplateHaskell #-}
{- | ==Usage

  In any file, you can specify tests above a function declaration, like:

  @
  --[list of args] [exceptedResult]
  --[1 2] [3]
  add x y = x+y
  @

  You can use any object deriving from Show and Eq as an argument for tests.
  Later, on your test file, you can build tests functions with

  @
  \{\-\# LANGUAGE TemplateHaskell \#\-\}
  $(makeAllTests "some\/Path\/File.hs")
  @

  and use the produced function in your main:

  @
  import System.Exit

  main :: IO ()
  main = do
    let (str,res) = _TEST_some_Path_File
    putStrLn str
    if res then exitSuccess else exitFailure
  @

  If you want to make tests on the actual file, you can use

  @
  $(makeAllTestsHere)
  @

  the function produced will be equivalent to the one produced by

  @
  $(makeAllTests "path\/to\/file\/known\/by\/ghc")
  @

  == String produced
  Considering the given file:

  @
  --[1 2] [3]
  --[1 2] [4]
  add x y = x+y
  @

  The string produced will be:

  @
  Test passed: add 1 2  == 3
  Error: add 1 2 /= 4 BUT == 3
  @

 -}

module Test.TestCom
    (makeAllTests,
    makeAllTestsHere
    ) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.Meta.Parse
import Language.Haskell.Meta.Utils
import Data.List
import Data.Either

data TestT = TestT {
  valueTest :: [String], -- list of args
  resTest :: [String], -- list of results
  testF :: String,
  actualU :: Int
} deriving (Show)

-- | With a path like some\/Path\/File.hs, Create a function
--
-- @
-- _TEST_some_Path_File :: (String,Bool)
-- @
--
-- with the string containing the result of all tests, and the boolean set to @True@ if and only if all tests passed
--
-- This also create sub-functions that each produce a Eihter String String
makeAllTests :: FilePath -> Q [Dec]
makeAllTests str = do
  let str' = (take ((length str)-3) (replaceXbyY str '/' '_'))
  file <- runIO $ readFile str
  funcs <- sequenceQ (buildTests str' (getTestT file))
  nd <- runTests str' $ appRecDec $ funcs
  return (nd : funcs)

makeAllTestsHere :: Q [Dec]
makeAllTestsHere = do
  loc <- location >>= (\(Loc y _ _ _ _) -> return y)
  makeAllTests loc

buildTests' :: String -> TestT -> [Q Dec]
buildTests' _ (TestT [] [] _ _) = []
buildTests' s x@(TestT valueTest' resTest' testF' actualU') = nd : buildTests' s nxs
  where
    nxs = x {valueTest = tail valueTest', resTest = tail resTest', actualU = actualU'+1}
    fname = mkName $ "_TEST_"++ s ++ testF' ++ show actualU' -- Tests have name like _TEST_funcnameX
    norm = either (const $ liftString "Failed to parse") return $ parseExp $ head resTest'
    res = if null (head valueTest') then varE $ mkName testF' else either (const $ liftString "Failed to parse") (\x -> return (appRec (reverse (unwindE x),VarE $ mkName testF'))) $ parseExp  $ head valueTest'
    guar1 = do
      a <- appE (appE ([| (==) |]) norm) res
      b <- appE [e|Right|] $ liftString (testF' ++ (if null (head valueTest') then [] else " ") ++ (head valueTest') ++ " == " ++ (head resTest'))
      return (NormalG a,b)
    guar2 = do
      a <- [e|otherwise|]
      b <- appE [e|Left|] $ appE (appE [e|(++)|] (liftString (testF' ++ " " ++ (head valueTest') ++ " /= " ++ (head resTest') ++ " BUT == "))) (appE [e|show|] res)
      return (NormalG a,b)
    fbody = guardedB [guar1,guar2]
    fClause = clause [] fbody []
    nd = funD fname [fClause]

buildTests :: String -> [TestT] -> [Q Dec]
buildTests _ [] = []
buildTests s (x:xs) = (buildTests' s x) ++ (buildTests s xs)

runTests :: String -> [Q Exp] -> Q Dec
runTests str funcs_runned = funD fname [fClause]
  where
    fname = mkName $ "_TEST_"++ str
    ex = appE (appE ([e|(++)|]) (appE [e|unlines|] (appE [e|builFinalString|] (listE funcs_runned)))) (([e|"TOTAL PASSED: " ++ show countRight' ++ "/"++ show length'|]))
    cr = valD (varP (mkName "countRight'")) (normalB (appE [e|countRight|] (listE funcs_runned))) []
    len = valD (varP (mkName "length'")) (normalB (appE [e|length|] (listE funcs_runned))) []
    boo = [e|countRight' == length'|]
    fClause = clause [] (normalB (tupE [ex,boo])) [cr,len]

appRec :: ([Exp],Exp) -> Exp
appRec ([],a) = a
appRec ((x:xs),a) = AppE (appRec (xs,a)) x

-- Run all declarations and store them into a tab
appRecDec :: [Dec] -> [Q Exp]
appRecDec [] = []
appRecDec (x:xs) = (varE (getName x)) : appRecDec xs

getName :: Dec -> Name
getName (FunD name _ ) = name

builFinalString :: [Either String String] -> [String]
builFinalString [] = [""]
builFinalString (x:xs) = (either ("Error: " ++ ) ("Test passed: " ++) x ): builFinalString xs

countRight :: [Either a b] -> Int
countRight z = foldl (\x y -> if isLeft y then x else x+1) (0 :: Int) z

getTestT :: String -> [TestT]
getTestT str = getTestT' (lines str) False (TestT [] [] [] 0)

-- t is supposed non empty
getTestT' :: [String] -> Bool -> TestT -> [TestT]
getTestT' [] _ _ = []
getTestT' (x:xs) b t
  | "--[" `isPrefixOf` x && "]" `isSuffixOf` x = getTestT' xs True $ t {valueTest = args : (valueTest t), resTest = res : (resTest t)}
  | not (null $ words x) && not ("--" `isPrefixOf` hw) && b = t {testF = hw} : getTestT' xs False (TestT [] [] [] 0)
  | otherwise = getTestT' xs b t
  where
    (fa,fb) = parenC x 0 (-1,0)
    (sa,sb) = parenC x (fb+1) (-1,0)
    args' = drop (fa+1) $ take fb x
    res' = drop (sa+1) $ take (sb) x
    args = if (sa,sb) == (0,0) then [] else args'
    res = if (sa,sb) == (0,0) then args' else res'
    hw = head (words x)

replaceXbyY :: String -> Char -> Char -> String
replaceXbyY [] _ _ = []
replaceXbyY (x:xs) a b
  | x == a = b:replaceXbyY xs a b
  | otherwise = x : replaceXbyY xs a b

-- To be call with 0 (-1,0)
parenC :: String -> Int -> (Int,Int) -> (Int, Int)
parenC str pos t@(i,j)
  | pos >= length str = (0,0)
  | str!!pos == '[' = parenC str (pos+1) ((if i== -1 then pos else i),j-1)
  | str!!pos == ']' = if j== -1 then (i,pos) else parenC str (pos+1) (i,j+1)
  | otherwise = parenC str (pos+1) t
