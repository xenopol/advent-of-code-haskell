import Day02
import Test.Tasty
import Test.Tasty.HUnit

main = do
  defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Day 2"
    [ testGroup "Part 1" $ buildTestCases part1 examplesPart1,
      testGroup "Part 2" $ buildTestCases part2 examplesPart2
    ]

type Examples = [([Char], Int)]

examplesPart1 :: Examples
examplesPart1 =
  [ ("2x3x4", 58),
    ("1x1x10", 43)
  ]

examplesPart2 :: Examples
examplesPart2 = [("2x3x4", 34), ("1x1x10", 14)]

buildTestCases :: (String -> Int) -> Examples -> [TestTree]
buildTestCases f =
  map
    ( \(pattern, result) ->
        testCase (pattern ++ " should equal " ++ (show result)) (f pattern @?= result)
    )
