{-# LANGUAGE TemplateHaskell #-}
module Main where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.Exit

import Test.TestCom

$(makeAllTests)

main :: IO ()
main = do
  let (str,res) = _TEST_tests_Main
  putStrLn str
  if res then exitSuccess else exitFailure
  -- print $ getTestT file

--[1 2 3]
add :: Int -> Int -> Int
add x y = x+y
