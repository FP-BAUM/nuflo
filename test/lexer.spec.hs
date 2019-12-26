import Test.HUnit

test_01 = TestCase (assertEqual "number del assert" True True)

tests = TestList [TestLabel "test01" test_01]
