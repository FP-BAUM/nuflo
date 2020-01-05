
module TestMain(runAllTests) where

import Test(runTestSuites)
import qualified TestLexer

runAllTests :: IO ()
runAllTests = runTestSuites [
                 TestLexer.tests
              ]

