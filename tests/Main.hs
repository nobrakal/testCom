{-# LANGUAGE TemplateHaskell #-}
module Main where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Test.TestCom

$(sequenceQ $ buildTests [TestT [["1","2"]] ["3"] "add" 0])

-- Tests
main :: IO ()
main = do
  putStrLn $ show _TEST_add0

add :: Int -> Int -> Int
add x y = x+y
