{-# LANGUAGE TemplateHaskell #-}

module Test.TestCom
    ( TestT (..),
  --  runTests,
  --  buildTests,
    makeAllTests
    ) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.List
import Data.Either

data TestT = TestT {
  valueTest :: [[String]], -- list of list of args
  resTest :: [String],
  testF :: String,
  actualU :: Int
}

makeAllTests :: [TestT] -> [Q Dec]
makeAllTests tab = nd : funcs
  where
    funcs = buildTests tab
    fname = mkName "_TEST_runAllTests"
    funcs_runned = appRecDec funcs
    ex = appE (appE ([e|(++)|]) (appE [e|unlines|] (appE [e|builFinalString|] (listE funcs_runned)))) (appE ([e|\z -> "TOTAL PASSED: " ++ show (countRight z) ++ "/"++ show (length z)|]) (listE funcs_runned))
    fClause = clause [] (normalB ex) []
    nd = funD fname [fClause]

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
buildTests (x:xs) = buildTests' x ++ buildTests xs

appRec :: ([String],Q Exp) -> Q Exp
-- appRec [] = return
appRec ([],a) = a
appRec ((x:xs),b) = appE (appRec (xs,b)) (litE $ integerL (read x))

-- Run all declarations and store them into a tab
appRecDec :: [Q Dec] -> [Q Exp]
appRecDec [] = []
appRecDec (x:xs) = (x >>= \y -> (varE (getName y))) : appRecDec xs

getName :: Dec -> Name
getName (FunD name _ ) = name

builFinalString :: [Either String String] -> [String]
builFinalString [] = [""]
builFinalString (x:xs) = (either ("Error: " ++ ) ("Test passed: " ++) x ): builFinalString xs

countRight :: [Either a b] -> Int
countRight z = foldl (\x y -> if isLeft y then x else x+1) (0 :: Int) z
