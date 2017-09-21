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
    fbody = guardedB [(normalG (appE (appE ([| (==) |]) (litE norm)) res), [e|Right True|]),(normalG  [e|otherwise|], appE [e|Left|] [e|$(testF')|] ) ]
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
