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

import Test.TestCom.Type
import Test.TestCom.Parser.Parser

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
makeAllTests str = makeAllTests' name str
  where
    name = take ((length str)-3) $ replaceXbyY '/' '_' str
    replaceXbyY a b = map (\x -> if x == a then b else x) 

makeAllTestsHere :: Q [Dec]
makeAllTestsHere = location >>= makeAllTests' "HERE" . loc_filename

makeAllTests' :: String -> FilePath -> Q [Dec]
makeAllTests' name fp = do
  file <- runIO $! getTestT fp
  case file of
    Left error -> fail $ show error
    Right testList -> do
      funcs <- sequenceQ $ concatMap (buildTests' name) testList
      nd <- runTests name $ map (varE . getName) funcs
      return (nd : funcs)

buildTests' :: String -> Test -> [Q Dec]
buildTests' _ (Test [] _ _) = []
buildTests' s x@(Test (t@(TestUnit actB actV actRes numOfT):testU') testF' actualU') = do
  let actAndResQ = if actB == Spec then makeRandom actV actRes testF' else return (actV, actRes)
  let guar1And2 = actAndResQ >>= \(x,y) -> do
                                            let r1 = actResByTestType y
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
  newVars <- sequenceQ $ map (generateRandomVars fname) $ extractVarsName first'
  return (unwordAndMap newVars first',unwordAndMap newVars $ words second)
  where
    first' = words first
    unwordAndMap newVars = unwords . map (replaceVarsByValue newVars)

extractVarsName :: [String] -> [(String,String)]
extractVarsName = map (\x -> (take (posOfArobase x) x, drop ((posOfArobase x)+1) x)) . filter (elem '@')
  where 
    posOfArobase = fromJust . elemIndex '@'

generateRandomVars :: String -> (String,String) -> Q (String,String)
generateRandomVars fname (name,typ) = do
      value <- case typ of
                "Int" -> runIO $ (randomIO :: IO Int) >>= paren
                "Bool" -> runIO $ (randomIO :: IO Bool) >>= paren
                "Char" -> runIO $ (randomIO :: IO Char) >>= paren
                otherwise -> fail $ "Bad type specified in the test of " ++ fname ++ " in the variable " ++ name ++ ": "++typ
      return (name,value)
  where
    paren x= return $ "(" ++ show x ++ ")"

replaceVarsByValue :: [(String,String)] -> String -> String
replaceVarsByValue tab x = case elemIndex '@' x of
  Just i -> case lookup (take i x) tab of
    Just a -> a
    Nothing -> x
  Nothing -> x

eith = either (\x -> fail $ "Failed to parse:" ++ show x)

calculatedRes :: (TestType,String) -> String -> ExpQ
calculatedRes (Override,actV) _ = eith return $ parseExp $ actV
calculatedRes (_,actV) testF
  | null actV = varE $ mkName testF
  | otherwise = eith (\x -> return (appRec (reverse (unwindE x),VarE $ mkName testF))) $ parseExp actV

actResByTestType :: String -> ExpQ
actResByTestType = eith return . parseExp

runTests :: String -> [Q Exp] -> Q Dec
runTests str funcs_runned = funD fname [fClause]
  where
    fname = mkName $ "_TEST_"++ str
    ex = appE (appE ([e|(++)|]) (appE [e|unlines|] (appE [e|map builFinalString|] (listE funcs_runned)))) (([e|"TOTAL PASSED: " ++ show countRight' ++ "/"++ show length'|]))
    cr = valD (varP (mkName "countRight'")) (normalB (appE [e|countRight|] (listE funcs_runned))) []
    len = valD (varP (mkName "length'")) (normalB (appE [e|length|] (listE funcs_runned))) []
    boo = [e|countRight' == length'|]
    fClause = clause [] (normalB (tupE [ex,boo])) [cr,len]

appRec :: ([Exp],Exp) -> Exp
appRec ([],a) = a
appRec ((x:xs),a) = AppE (appRec (xs,a)) x

getName :: Dec -> Name
getName (FunD name _ ) = name

builFinalString :: Either String String -> String
builFinalString  = either ("Test Errored: " ++ ) ("Test passed: " ++)

countRight :: [Either a b] -> Int
countRight = length . filter isRight 
