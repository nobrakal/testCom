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
