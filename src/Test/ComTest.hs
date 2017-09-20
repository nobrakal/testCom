{-# LANGUAGE TemplateHaskell #-}

module ComTest
    ( runTests
    ) where

runTests :: FilePath -> IO Bool
runTests fp = do
  file <- readFile fp
  return True

keepCommentsAndFollowing :: [String] -> Bool -> [[String]]
keepCommentsAndFollowing [] _ = [[]]
keepCommentsAndFollowing (x:xs) b
  | foldl (\x y -> x == " " && y) True x = keepCommentsAndFollowing xs b --Remove empty lines
  | "--" `isPrefixOf` x = words (drop 2 x) : keepCommentsAndFollowing xs True
  | b = words x : keepCommentsAndFollowing xs False
