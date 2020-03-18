
module TestMain(runAllTests) where

import Test(runTestsAndSummarize)
import qualified TestLexer
import qualified TestModuleSystem
import qualified TestParser
import qualified TestKindInference

runAllTests :: IO ()
runAllTests = runTestsAndSummarize [
                TestLexer.tests
              , TestModuleSystem.tests
              , TestParser.tests
              , TestKindInference.tests
              ]

