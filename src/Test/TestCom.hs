{-# LANGUAGE TemplateHaskell #-}
{- | ==Usage

  === Write your tests
  You can use any object deriving from Show and Eq as an argument for tests.
  ==== Normal Tests
  In any file, you can specify tests above a function declaration, like:

  @
  --[list of args] [exceptedResult]
  --[1 2] [3]
  add x y = x+y
  @

  ==== Overrided tests
  With this syntax, testCom will only compare the two provided expression

  @
  --O[Expression with the same type than the result] [exceptedResult]
  --O[add 1 2] [3]
  add x y = x+y
  @

  ==== Tests by specification

  @
  --S[expressionInvolvingYourFunction] [OtherExpression] [Integer]
  --S[x@Int y@Int] [x@ + y@] [100]
  add x y = x+y
  @

  Here, testCom will build N tests with random arguments (specified by nameOfTheArgs@Type). Random arguments MUST be separated by spaces. For now, only base types are supported: Char, Int and Bool

  === Build your tests

  Later, on your test file, you can build tests functions with

  @
  \{\-\# LANGUAGE TemplateHaskell \#\-\}
  $(makeAllTests "some\/Path\/File.hs")
  @

  and use the produced function in your main:

  @
  import System.Exit

  main :: IO ()
  main = do
    let (str,res) = _TEST_some_Path_File
    putStrLn str
    if res then exitSuccess else exitFailure
  @

  If you want to make tests on the actual file, you can use

  @
  $(makeAllTestsHere)
  @

  the function produced will be equivalent to the one produced by

  @
  $(makeAllTests "path\/to\/file\/known\/by\/ghc")
  @

  == String produced
  Considering the given file:

  @
  --[1 2] [3]
  --[1 2] [4]
  add x y = x+y
  @

  The string produced will be:

  @
  Test passed: add 1 2  == 3
  Error: add 1 2 /= 4 BUT == 3
  @

 -}

module Test.TestCom
    (makeAllTests,
    makeAllTestsHere
    ) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.Meta.Parse
import Language.Haskell.Meta.Utils
import Data.List
import Data.Either
import Data.Maybe (fromJust)
import System.Random

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

-- | With a path like some\/Path\/File.hs, Create a function
--
-- @
-- _TEST_some_Path_File :: (String,Bool)
-- @
--
-- with the string containing the result of all tests, and the boolean set to @True@ if and only if all tests passed
--
-- This also create sub-functions that each produce a Eihter String String
makeAllTests :: FilePath -> Q [Dec]
makeAllTests str = do
  let str' = (take ((length str)-3) (replaceXbyY str '/' '_'))
  file <- runIO $ readFile str
  funcs <- sequenceQ (buildTests str' (getTestT file))
  nd <- runTests str' $ appRecDec $ funcs
  return (nd : funcs)

makeAllTestsHere :: Q [Dec]
makeAllTestsHere = do
  loc <- location >>= (\(Loc y _ _ _ _) -> return y)
  makeAllTests loc

buildTests' :: String -> Test -> [Q Dec]
buildTests' _ (Test [] _ _) = []
buildTests' s x@(Test (t@(TestUnit actB actV actRes numOfT):testU') testF' actualU') = do
  let actAndResQ = if actB == Spec then makeRandom actV actRes testF' else return (actV, actRes)
  let guar1And2 = actAndResQ >>= \(x,y) -> do
                                            let r1 = actResByTestType actB y
                                            let r2 = calculatedRes (actB,x) testF'
                                            a1 <- appE (appE ([| (==) |]) r1) r2
                                            b1 <- (appE [e|Right|] $ liftString ((if not isNormal then [] else testF') ++ (if (null (x)) || not isNormal then [] else " ") ++ x  ++ " == " ++ y))
                                            a2 <- [e|otherwise|]
                                            b2 <- ( appE [e|Left|] $ appE (appE [e|(++)|] (liftString (testF' ++ " " ++ x ++ " /= " ++ y ++ " BUT == "))) (appE [e|show|] r2))
                                            return (return (NormalG a1, b1), return (NormalG a2,b2))
  let fClause = guar1And2 >>= \(guar1,guar2) -> clause [] (guardedB [guar1,guar2]) []
  (funD fname [fClause]) : buildTests' s nxs
  where
    nxs = (if numOfT == 1 then x {testU = testU'} else x {testU = (t {numOfTests = numOfT-1}):testU'}) {actualU = actualU'+1}
    fname = mkName $ "_TEST_"++ s ++ testF' ++ show actualU' -- Tests have name like _TEST_funcnameX
    isNormal = case actB of
      Override -> False
      otherwise -> True

makeRandom :: String -> String -> String -> Q(String, String)
makeRandom first second fname = do
  newVars <- sequenceQ $ generateRandomVars fname $ extractVarsName first'
  let first'' = unwords $ replaceVarsByValue first' (newVars)
  let second' = unwords $ replaceVarsByValue (words second) (newVars)
  return (first'',second')
  where
    first' = words first

extractVarsName :: [String] -> [(String,String)]
extractVarsName [] = []
extractVarsName (x:xs)
  | '@' `elem` x = (take posOfArobase x, drop (posOfArobase+1) x) : extractVarsName xs
  | otherwise = extractVarsName xs
  where
    posOfArobase = fromJust $ elemIndex '@' x

generateRandomVars :: String -> [(String,String)] -> [Q (String,String)]
generateRandomVars _ [] = []
generateRandomVars fname ((name,typ):xs) = do
  res : generateRandomVars fname xs
  where
    paren x= return $ "(" ++ show x ++ ")"
    res = do
      value <- case typ of
                "Int" -> runIO $ (randomIO :: IO Int) >>= paren
                "Bool" -> runIO $ (randomIO :: IO Bool) >>= paren
                "Char" -> runIO $ (randomIO :: IO Char) >>= paren
                otherwise -> fail $ "Bad type specified in the test of " ++ fname ++ " in the variable " ++ name ++ ": "++typ
      return (name,value)


replaceVarsByValue :: [String] -> [(String,String)] -> [String]
replaceVarsByValue [] _ = []
replaceVarsByValue (x:xs) tab
  | '@' `elem` x = case lookup (take posOfArobase x) tab of
    Just a -> a :replaceVarsByValue xs tab
    Nothing -> x : replaceVarsByValue xs tab
  | otherwise = x : replaceVarsByValue xs tab
  where
    posOfArobase = fromJust $ elemIndex '@' x

eith = either (\x -> fail $ "Failed to parse:" ++ show x)

calculatedRes :: (TestType,String) -> String -> ExpQ
calculatedRes (Override,actV) _ = eith return $ parseExp $ actV
calculatedRes (_,actV) testF
  | null (actV) = varE $ mkName testF
  | otherwise = eith (\x -> return (appRec (reverse (unwindE x),VarE $ mkName testF))) $ parseExp $ actV
--calculatedRes (Spec,actV) _ = [e|True|]

actResByTestType :: TestType -> String -> ExpQ
actResByTestType _ ar = eith return $ parseExp ar

buildTests :: String -> [Test] -> [Q Dec]
buildTests _ [] = []
buildTests s (x:xs) = (buildTests' s x) ++ (buildTests s xs)

runTests :: String -> [Q Exp] -> Q Dec
runTests str funcs_runned = funD fname [fClause]
  where
    fname = mkName $ "_TEST_"++ str
    ex = appE (appE ([e|(++)|]) (appE [e|unlines|] (appE [e|builFinalString|] (listE funcs_runned)))) (([e|"TOTAL PASSED: " ++ show countRight' ++ "/"++ show length'|]))
    cr = valD (varP (mkName "countRight'")) (normalB (appE [e|countRight|] (listE funcs_runned))) []
    len = valD (varP (mkName "length'")) (normalB (appE [e|length|] (listE funcs_runned))) []
    boo = [e|countRight' == length'|]
    fClause = clause [] (normalB (tupE [ex,boo])) [cr,len]

appRec :: ([Exp],Exp) -> Exp
appRec ([],a) = a
appRec ((x:xs),a) = AppE (appRec (xs,a)) x

-- Run all declarations and store them into a tab
appRecDec :: [Dec] -> [Q Exp]
appRecDec [] = []
appRecDec (x:xs) = (varE (getName x)) : appRecDec xs

getName :: Dec -> Name
getName (FunD name _ ) = name

builFinalString :: [Either String String] -> [String]
builFinalString [] = [""]
builFinalString (x:xs) = (either ("Error: " ++ ) ("Test passed: " ++) x ): builFinalString xs

countRight :: [Either a b] -> Int
countRight z = foldl (\x y -> if isLeft y then x else x+1) (0 :: Int) z

getTestT :: String -> [Test]
getTestT str = getTestT' (lines str) False (Test [] [] 0)

-- t is supposed non empty
getTestT' :: [String] -> Bool -> Test -> [Test]
getTestT' [] _ _ = []
getTestT' (x:xs) b t
  | "--" `isPrefixOf` x && (isStartingWith' "[" || isStartingWith' "O[" || isStartingWith' "S[" ) && isStartingWith (reverse x) "]" = getTestT' xs True (t {testU = (TestUnit tesT args res nbOfTests) : (testU t)})
  | not (null $ words x) && not ("--" `isPrefixOf` hw) && b = t {testF = hw} : getTestT' xs False (Test [] [] 0)
  | otherwise = getTestT' xs b t
  where
    isStartingWith' = isStartingWith (drop 2 x)
    tesT = if isStartingWith' "[" then Normal else if isStartingWith' "O[" then Override else Spec
    nbOfTests = if tesT == Spec then read (drop (ta+1) $ take (tb) x) else 1
    (fa,fb) = parenC x 0 (-1,0)
    (sa,sb) = parenC x (fb+1) (-1,0)
    (ta,tb) = parenC x (sb+1) (-1,0)
    args' = drop (fa+1) $ take fb x
    res' = drop (sa+1) $ take sb x
    args = if (sa,sb) == (0,0) then [] else args'
    res = if (sa,sb) == (0,0) then args' else res'
    hw = head (words x)

isStartingWith :: String -> String -> Bool
isStartingWith [] _ = False
isStartingWith _ [] = True
isStartingWith (x:xs) s@(x':xs')
  | x == ' ' = isStartingWith xs s
  | x == x' = True && isStartingWith xs xs'
  | otherwise = False

replaceXbyY :: String -> Char -> Char -> String
replaceXbyY [] _ _ = []
replaceXbyY (x:xs) a b
  | x == a = b:replaceXbyY xs a b
  | otherwise = x : replaceXbyY xs a b

-- To be call with 0 (-1,0)
parenC :: String -> Int -> (Int,Int) -> (Int, Int)
parenC str pos t@(i,j)
  | pos >= length str = (0,0)
  | str!!pos == '[' = parenC str (pos+1) ((if i== -1 then pos else i),j-1)
  | str!!pos == ']' = if j== -1 then (i,pos) else parenC str (pos+1) (i,j+1)
  | otherwise = parenC str (pos+1) t
