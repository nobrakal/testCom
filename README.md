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
import Test.TestCom

$(sequenceQ $ makeAllTests fileToTest.hs)

-- Tests
main :: IO ()
main = do
  putStrLn _TEST_runAllTests
```
