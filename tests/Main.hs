{-# LANGUAGE TemplateHaskell #-}
module Main where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.Exit

import Test.TestCom

data Point = Point Int Int deriving (Show,Eq)

$(makeAllTestsHere)

main :: IO ()
main = do
  let (str,res) = _TEST_tests_Main
  putStrLn str
  if res then exitSuccess else exitFailure

--[1 2] [3]
--S[@x @y] [@x+@y] [100]
add :: Int -> Int -> Int
add x y = x+y

--['a' 'b'] ["ab"]
add' :: Char -> Char -> String
add' x y = [x,y]

--[(1,2) (2,3)] [(3,5)]
add'' :: (Int,Int) -> (Int,Int) -> (Int,Int)
add'' (x,y) (x',y') = (x+x',y+y')

--[[1,2] [3]] [[1,2,3]]
add''' :: [Int] -> [Int] -> [Int]
add''' x y = x ++ y

--[add 1 2] [3]
add'''' :: (Int -> Int -> Int) -> Int -> Int -> Int
add'''' f a b = f a b

-- [2]
-- O[onlyOne][2]
onlyOne :: Int
onlyOne = 2

--[(Point 1 2) (Point 2 3)] [(Point 3 5)]
add''''' :: Point -> Point -> Point
add''''' (Point y x) (Point y' x') = Point (y+y') (x+x')
