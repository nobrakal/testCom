{-# LANGUAGE TemplateHaskell #-}

module Test.TestCom
    ( TestT (..),
  --  runTests,
    buildTests
    ) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.List

data TestT = TestT {
  valueTest :: [[String]], -- list of list of args
  resTest :: [String],
  testF :: String,
  actualU :: Int
}

{- runTests tab = funcs ++ nd
  where
    funcs = buildTests tab
    fname = mkName "runAllTests"
    fClause = clause [] (guardedB gb) []
    nd = funD fname [fClause] -}

buildTests' :: TestT -> [Q Dec]
buildTests' (TestT [] _ _ _) = []
buildTests' x@(TestT valueTest' resTest' testF' actualU') = nd : buildTests' nxs
  where
    nxs = x {valueTest = tail valueTest', resTest = tail resTest', actualU = actualU'+1}
    fname = mkName $ "_TEST_" ++ testF' ++ show actualU'
    norm = integerL $ read $ head resTest'
    res = (appRec (head valueTest', (varE $ mkName testF')))
    guar1 = do
      a <- appE (appE ([| (==) |]) (litE norm)) res
      b <- [e|Right True|]
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

{- appRecAnd :: [Q Dec] -> Q Exp
appRecAnd [x] = x >>= \y -> varE (getName y)
appRecAnd  (x:xs) = x >>= \y -> appE (appE [|(&&)|] (appRecAnd xs)) (varE (getName y))

getName :: Dec -> Name
getName (FunD name _ ) = name
-}
