## testCom

### How to use it

#### Write your tests

Above a function to test, write your test like this:

```
--[1 2 3]
add :: Int -> Int -> Int
add x y = x+y
```

#### Run them
```
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.Exit

$(sequenceQ $ makeAllTests fileToTest.hs)

-- Tests
main :: IO ()
main = do
  let (str,res) = _TEST_runAllTests
  putStrLn str
  if res then exitSuccess else exitFailure
```
