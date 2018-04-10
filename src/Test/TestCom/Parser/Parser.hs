module Test.TestCom.Parser.Parser
--(getTestT)
where

import Text.ParserCombinators.Parsec hiding ((<|>), many)
import qualified Text.Parsec.Combinator
import Text.Parsec.Char (endOfLine)
import Control.Applicative
import Control.Monad
import Data.Semigroup

import Test.TestCom.Type

(<||>) :: Parser a -> Parser a -> Parser a
a <||> b = (try a) <|> b

inner :: Parser String
inner = string "[" <> many (noneOf "[]") <> (lookAhead (string "]") <|> inner ) <> string "]"

testUnit :: Parser TestUnit
testUnit =      (lookAhead (char '[') *> normal)
           <||> (char 'O' *> override)
           <||> (char 'S' *> spec)
  where
    normal = do
      args <- inner
      skipMany space
      result <- inner
      return $ TestUnit Normal args result 1
    override = do
      args <- inner
      skipMany space
      result <- inner
      return $ TestUnit Override args result 1
    spec = do
      args <- inner
      skipMany space
      result <- inner
      skipMany space
      nb <- ((\x -> read x :: Int) <$> inner) <||> (return 1)
      return $ TestUnit Spec args result nb

tests :: Parser [Test]
tests = notTest *> sepEndBy test notTest
  where
    notTest = manyTill (anyChar) (lookAhead test) 

test :: Parser Test
test = do
  tests <- string "--" *> testUnit `endBy` endOfLine 
  func <- manyTill (anyChar) (try (string " ")) <* (many (noneOf "\n") >> endOfLine)
  return $ Test tests func 0

getTestT :: String -> IO (Either ParseError [Test])
getTestT filename = do
  content <- readFile filename
  return $ parse tests filename content

