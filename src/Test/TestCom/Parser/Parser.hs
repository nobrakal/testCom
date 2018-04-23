module Test.TestCom.Parser.Parser
--(getTestT)
where

import Text.ParserCombinators.Parsec hiding ((<|>), many)
import qualified Text.Parsec.Combinator
import Text.Parsec.Char (endOfLine)
import Control.Applicative
import Control.Monad
import Data.Semigroup hiding (option)

import Test.TestCom.Type

(<||>) :: Parser a -> Parser a -> Parser a
a <||> b = (try a) <|> b

inner :: Parser String
inner = string "[" <> many (noneOf "[]") <> (lookAhead (string "]") <|> inner ) <> string "]"

starter :: Parser TestType
starter = (lab >> return Normal)
           <||> (char 'O' *>lab >> return Override)
           <||> (char 'S' *> lab >> return Spec)
  where
    lab = lookAhead (char '[') 


testUnit :: TestType -> Parser TestUnit
testUnit tt = do
    args <- inner
    skipMany space
    result <- inner
    case tt of
      Normal -> return $ TestUnit Normal args result 1
      Override -> return $ TestUnit Override args result 1
      Spec -> do
        skipMany space
        nb <- option 1 $ (\x -> read x :: Int) <$> inner
        return $ TestUnit Spec args result nb

tests :: Parser [Test]
tests = notTest *> sepEndBy test notTest
  where
    notTest = manyTill anyChar (lookAhead test) 

test :: Parser Test
test = do
  tests <- (string "--" *> starter >>= testUnit) `endBy` endOfLine 
  func <- manyTill anyChar (try (string " ")) <* (many (noneOf "\n") >> endOfLine)
  return $ Test tests func 0

getTestT :: String -> IO (Either ParseError [Test])
getTestT filename = do
  content <- readFile filename
  return $ parse tests filename content

