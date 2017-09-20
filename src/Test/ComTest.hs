{-# LANGUAGE TemplateHaskell #-}

module TestCom
    ( runTests
    ) where

data TestT = TestT {
  testsN :: [[String]] ,
  testF :: String
}

runTests :: FilePath -> IO Bool
runTests fp = do
  file <- readFile fp
  let comm = keepCommentsAndFollowing (lines file) False [TestT [[]] ""]
  return True

keepCommentsAndFollowing :: [String] -> Bool -> [TestT] -> [TestT]
keepCommentsAndFollowing [] _ tab = tab
keepCommentsAndFollowing (x:xs) b tab
  | foldl (\x y -> x == " " && y) True x = keepCommentsAndFollowing xs b --Remove empty lines
  | "--" `isPrefixOf` x = keepCommentsAndFollowing xs True $ head tab {testsN = testsN tab ++ words x} : tab
  | b = keepCommentsAndFollowing xs False $ head tab {testF = x} : tail tab
  | otherwise  = keepCommentsAndFollowing xs b
