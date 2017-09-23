{-# LANGUAGE TemplateHaskell #-}

module Test.TestCom
    ( TestT (..),
    makeAllTests,
    getTestT
    ) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.List
import Data.Either

data TestT = TestT {
  valueTest :: [[String]], -- list of list of args
  resTest :: [String], -- list of results
  testF :: String,
  actualU :: Int
} deriving (Show)

makeAllTests :: Q [Dec]
makeAllTests = do
  loc <- location >>= (\(Loc y _ _ _ _) -> return y)
  file <- runIO $ readFile loc
  funcs <- sequenceQ (buildTests (getTestT file))
  nd <- runTests loc $ appRecDec $ funcs
  return (nd : funcs)

buildTests' :: TestT -> [Q Dec]
buildTests' (TestT [] _ _ _) = []
buildTests' x@(TestT valueTest' resTest' testF' actualU') = nd : buildTests' nxs
  where
    nxs = x {valueTest = tail valueTest', resTest = tail resTest', actualU = actualU'+1}
    fname = mkName $ "_TEST_" ++ testF' ++ show actualU' -- Tests have name like _TEST_funcnameX
    norm = integerL $ read $ head resTest' -- For now only integer
    res = (appRec (head valueTest', (varE $ mkName testF')))
    guar1 = do
      a <- appE (appE ([| (==) |]) (litE norm)) res
      b <- appE [e|Right|] $ litE (stringL (testF' ++ " " ++ unwords (head valueTest') ++ " == " ++ (head resTest')))
      return (NormalG a,b)
    guar2 = do
      a <- [e|otherwise|]
      b <- appE [e|Left|] $ litE (stringL (testF' ++ " " ++ unwords (head valueTest') ++ " /= " ++ (head resTest')))
      return (NormalG a,b)
    fbody = guardedB [guar1,guar2]
    fClause = clause [] fbody []
    nd = funD fname [fClause]

buildTests :: [TestT] -> [Q Dec]
buildTests [] = []
buildTests (x:xs) = (buildTests' x) ++ (buildTests xs)

runTests :: String -> [Q Exp] -> Q Dec
runTests str funcs_runned = funD fname [fClause]
  where
    fname = mkName $ "_TEST_"++ take ((length str)-3) (replaceSlashByUnderscore str)
    ex = appE (appE ([e|(++)|]) (appE [e|unlines|] (appE [e|builFinalString|] (listE funcs_runned)))) (([e|"TOTAL PASSED: " ++ show countRight' ++ "/"++ show length'|]))
    cr = valD (varP (mkName "countRight'")) (normalB (appE [e|countRight|] (listE funcs_runned))) []
    len = valD (varP (mkName "length'")) (normalB (appE [e|length|] (listE funcs_runned))) []
    boo = [e|countRight' == length' |]
    fClause = clause [] (normalB (tupE [ex,boo])) [cr,len]

appRec :: ([String],Q Exp) -> Q Exp
-- appRec [] = return
appRec ([],a) = a
appRec ((x:xs),b) = appE (appRec (xs,b)) (litE $ integerL (read x))

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
  | not (null $ words x) && not ("--" `isPrefixOf` hw) && b = t {testF = hw} : getTestT' xs False (TestT [[]] [] [] 0)
  | otherwise = getTestT' xs b t
  where
    list = words $ drop 3 (init x)
    args = init list
    res = last list
    hw = head (words x)

replaceSlashByUnderscore :: String -> String
replaceSlashByUnderscore [] = []
replaceSlashByUnderscore (x:xs)
  | x == '/' = '_':replaceSlashByUnderscore xs
  | otherwise = x : replaceSlashByUnderscore xs
