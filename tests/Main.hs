{-# LANGUAGE TemplateHaskell #-}
module Main where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.Exit

import Test.TestCom

$(makeAllTestsHere)

main :: IO ()
main = do
  let (str,res) = _TEST_tests_Main
  putStrLn str
  if res then exitSuccess else exitFailure
  -- print $ getTestT file

--[1 2 3]
add :: Int -> Int -> Int
add x y = x+y

--['a' 'b' "ab"]
add' :: Char -> Char -> String
add' x y = [x,y]

--[(1,2) (2,3) (3,5)]
add'' :: (Int,Int) -> (Int,Int) -> (Int,Int)
add'' (x,y) (x',y') = (x+x',y+y')

--[[1,2] [3] [1,2,3]]
add''' :: [Int] -> [Int] -> [Int]
add''' x y = x ++ y

--[add 1 2 5]
add'''' :: (Int -> Int -> Int) -> Int -> Int -> Int
add'''' f a b = f a b

--[2]
onlyOne :: Int
onlyOne = 2
