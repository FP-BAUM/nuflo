
module TestMain(runAllTests) where

import Test(runTestsAndSummarize)
import qualified TestLexer
import qualified TestParser
import qualified TestModuleSystem

runAllTests :: IO ()
runAllTests = runTestsAndSummarize [
                TestLexer.tests
              , TestParser.tests
              , TestModuleSystem.tests
              ]

