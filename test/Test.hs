{-# LANGUAGE ExistentialQuantification #-}

module Test(TestSuite(..), Test(..), runTestSuites) where

data TestSuite = TestSuite {
                   testSuite_name  :: String
                 , testSuite_tests :: [Test]
                 }

data Test = forall a. (Eq a, Show a) =>
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

runTest :: Test -> IO TestSummary
runTest (TestCase name obtained expected) = do
  putStrLn ("[ TEST   ] " ++ name)
  if obtained == expected
   then do putStrLn ("[     OK ] " ++ name)
           return (TS 1 1)
   else do putStrLn ("[ FAILED ] " ++ name)
           putStrLn ("  Obtained: " ++ show obtained)
           putStrLn ("  Expected: " ++ show expected)
           return (TS 1 0)

runTests :: [Test] -> IO TestSummary
runTests tests = do
  summaries <- mapM runTest tests
  return $ tsSum summaries

runTestSuite :: TestSuite -> IO TestSummary
runTestSuite (TestSuite name tests) = do
  putStrLn ("[========] " ++ name)
  summary <- runTests tests
  putStrLn ("[========] " ++ name)
  return summary

runTestSuites :: [TestSuite] -> IO ()
runTestSuites testSuites = do
  TS total ok <- tsSum <$> mapM runTestSuite testSuites
  putStrLn ("Total : " ++ show total)
  putStrLn ("OK    : " ++ show ok)
  putStrLn ("FAILED: " ++ show (total - ok))
  if total == ok
   then putStrLn "All tests OK."
   else return ()

