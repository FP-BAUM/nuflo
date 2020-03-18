{-# LANGUAGE ExistentialQuantification #-}

module Test(Test(..), runTestsAndSummarize) where

import System.Exit(exitFailure)

data Test = TestSuite {
              testSuite_name  :: String
            , testSuite_tests :: [Test]
            }
          | forall a. (Eq a, Show a) =>
              TestCase {
                testCase_name     :: String
              , testCase_obtained :: a
              , testCase_expected :: a
              }

data TestSummary = TS {
                     testSummary_total :: Integer
                   , testSummary_ok    :: Integer
                   }

tsZero = TS 0 0
tsPlus (TS a1 b1) (TS a2 b2) = TS (a1 + a2) (b1 + b2)
tsSum = foldr tsPlus tsZero

spaces :: Int -> String
spaces n = replicate (2 * n) ' '

runTest :: Int -> Test -> IO TestSummary
runTest indentation (TestSuite name tests) = do
    putStrLn (margin ++ title indentation ++ " " ++ name)
    summary <- runTests (indentation + 1) tests
    return summary
  where
    margin = spaces indentation
    title 0 = "\n[Test Suite]"
    title 1 = "====="
    title 2 = "----"
    title 3 = "=="
    title 4 = "--"
    title 5 = "="
    title _ = "-"
   
runTest indentation (TestCase name obtained expected) = do
  if obtained == expected
   then do putStrLn (margin ++ "[ OK ] " ++ name)
           return (TS 1 1)
   else do putStrLn (margin ++ "[ FAILED ] " ++ name)
           putStrLn (margin ++ "Expected: " ++ show expected)
           putStrLn (margin ++ "Obtained: " ++ show obtained)
           return (TS 1 0)
  where margin = spaces indentation

runTests :: Int -> [Test] -> IO TestSummary
runTests indentation tests = do
  summaries <- mapM (runTest indentation) tests
  return $ tsSum summaries

separator :: String
separator = "----------------------------------------"

runTestsAndSummarize :: [Test] -> IO ()
runTestsAndSummarize tests = do
  putStrLn separator
  TS total ok <- runTests 0 tests
  putStrLn separator
  putStrLn (" TOTAL : " ++ show total)
  putStrLn (" OK    : " ++ show ok)
  putStrLn (" FAILED: " ++ show (total - ok))
  if total == ok
   then putStrLn "All tests OK."
   else exitFailure
  putStrLn separator

