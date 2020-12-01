import Day04
import Test.Tasty
import Test.Tasty.HUnit

main = do
  defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Day 4"
    [ testGroup "Part 1" $ buildTestCases part1 examplesPart1
    ]

type Examples = [([Char], Int)]

examplesPart1 :: Examples
examplesPart1 =
  [ ("abcdef", 609043),
    ("pqrstuv", 1048970)
  ]

buildTestCases :: (String -> Int) -> Examples -> [TestTree]
buildTestCases f =
  map
    ( \(pattern, result) ->
        testCase (pattern ++ " should equal " ++ (show result)) (f pattern @?= result)
    )
