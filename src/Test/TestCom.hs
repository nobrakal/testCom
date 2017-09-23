{-# LANGUAGE TemplateHaskell #-}

module Test.TestCom
    ( TestT (..),
    makeAllTests,
    makeAllTestsHere,
    getTestT
    ) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.List
import Data.Either
import Data.Char
import Data.Maybe (fromJust)

data TestT = TestT {
  valueTest :: [[String]], -- list of list of args
  resTest :: [String], -- list of results
  testF :: String,
  actualU :: Int
} deriving (Show)

makeAllTests :: String -> Q [Dec]
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
buildTests' _ (TestT [] _ _ _) = []
buildTests' s x@(TestT valueTest' resTest' testF' actualU') = nd : buildTests' s nxs
  where
    nxs = x {valueTest = tail valueTest', resTest = tail resTest', actualU = actualU'+1}
    fname = mkName $ "_TEST_"++ s ++ testF' ++ show actualU' -- Tests have name like _TEST_funcnameX
    norm = getLitE $ head resTest' -- For now only integer
    res = (appRec (reverse (head valueTest'), (varE $ mkName testF')))
    guar1 = do
      a <- appE (appE ([| (==) |]) norm) res
      b <- appE [e|Right|] $ litE (stringL (testF' ++ " " ++ unwords (head valueTest') ++ " == " ++ (head resTest')))
      return (NormalG a,b)
    guar2 = do
      a <- [e|otherwise|]
      b <- appE [e|Left|] $ appE (appE [e|(++)|] (litE (stringL (testF' ++ " " ++ unwords (head valueTest') ++ " /= " ++ (head resTest') ++ " BUT ")))) (appE [e|show|] res)
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
    boo = [e|countRight' == length' |]
    fClause = clause [] (normalB (tupE [ex,boo])) [cr,len]

appRec :: ([String],Q Exp) -> Q Exp
appRec ([],a) = a
appRec ((x:xs),b) = appE (appRec (xs,b)) (getLitE x)

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
    list = words $ drop 3 (init x)
    args = init list
    res = last list
    hw = head (words x)

replaceXbyY :: String -> Char -> Char -> String
replaceXbyY [] _ _ = []
replaceXbyY (x:xs) a b
  | x == a = b:replaceXbyY xs a b
  | otherwise = x : replaceXbyY xs a b

getLitE :: String -> Q Exp
getLitE str
  | head str == '\'' && last str == '\'' = litE $ charL $ str !! 1 -- Char
  | head str == '\"' && last str == '\"' = litE $ stringL btw -- String
  | (foldl (\y x -> y && isDigit x ) True str) = litE $ integerL (read str) -- Integer
  | '%' `elem` str = litE $ rationalL $ read str -- Ratio
  | head str == '(' && last str == ')' = tupE [getLitE fst', getLitE snd'] -- Tuple
  | head str == '[' && last str == ']' = listE $ getList $ words $ replaceXbyY btw ',' ' ' -- Tuple
  | otherwise = appE [e|read|] (litE $ stringL str) -- Par anything else with read
  where
    btw = init $ tail $ str
    (fst',_:snd') = splitAt (fromJust (elemIndex ',' btw)) btw
    getList [] = []
    getList (x:xs) = getLitE x : getList xs
