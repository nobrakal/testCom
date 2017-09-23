{-# LANGUAGE TemplateHaskell #-}
module Main where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import PseudoMacros
import System.Exit

import Test.TestCom

$(sequenceQ $ makeAllTests [TestT [["1","2"]] ["4"] "add" 0])

-- Tests
main :: IO ()
main = do
  let (str,res) = _TEST_runAllTests
  putStrLn str
  if res then exitSuccess else exitFailure

add :: Int -> Int -> Int
add x y = x+y
