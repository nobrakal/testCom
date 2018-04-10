module Test.TestCom.Type
where

data TestType = Normal | Override | Spec deriving (Show, Eq)

data TestUnit = TestUnit {
  typeOfT :: TestType,
  args :: String,
  result :: String,
  numOfTests :: Int
} deriving (Show)

data Test = Test {
  testU :: [TestUnit], -- list of (is the test normal,args) and the number of tests to do.
  testF :: String,
  actualU :: Int
} deriving (Show)
