
module TestMain(runAllTests) where

import Test(runTestSuites)
import qualified TestLexer
import qualified TestParser
import qualified TestModuleSystem

runAllTests :: IO ()
runAllTests = runTestSuites [
                TestLexer.tests
              , TestParser.tests
              , TestModuleSystem.tests
              ]

