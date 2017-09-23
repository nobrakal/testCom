{-# LANGUAGE TemplateHaskell #-}
module Main where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import PseudoMacros

import Test.TestCom

$(sequenceQ $ makeAllTests [TestT [["1","2"]] ["4"] "add" 0])

-- Tests
main :: IO ()
main = do
  putStrLn _TEST_runAllTests

add :: Int -> Int -> Int
add x y = x+y
