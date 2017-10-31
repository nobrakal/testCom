## testCom

### How to use it

#### Write your tests

Above a function to test, write your tests like this:

```
--[1 2] [3]
--O[add 1 2] [3]
--S[x@Int y@Int] [x@ + y@] [10]
add :: Int -> Int -> Int
add x y = x+y
```

#### Syntax

* Without any prefix: [args For The Function] [ExpectedResult]
* With a O (Override) prefix: [custom Function To Test] [ExpectedResult]
* With a S (Specification) prefix: [args Involving variable@Type] [ExpectedResult Maybe Involving variable@] [numberOfTestToDo]
(for now only basic types are supported: Int Char and Bool). variable@Type MUST be surrounded by spaces.

#### Run them
For example
```
{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.Exit

$(makeAllTestsHere)
$(makeAllTests "some/Path/File.hs")

-- Tests
main :: IO ()
main = do
  let (str,res) = _TEST_path -- Here, path=directory_actualfile. If your file is put in tests/Main.hs, then path=tests_Main (without the ".hs")
  let (str',res') = _TEST_some_Path_File
  putStrLn str
  putStrLn str'
  if res && res' then exitSuccess else exitFailure
```
