import Day01
import Test.Tasty
import Test.Tasty.HUnit

main = do
  defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Day 1"
    [ testGroup "Part 1" $ buildTestCases part1 examplesPart1,
      testGroup "Part 2" $ buildTestCases part2 examplesPart2
    ]

type Examples = [([Char], Int)]

examplesPart1 :: Examples
examplesPart1 =
  [ ("(())", 0),
    ("()()", 0),
    ("(((", 3),
    ("(()(()(", 3),
    ("))(((((", 3),
    ("())", -1),
    ("))(", -1),
    (")))", -3),
    (")())())", -3)
  ]

examplesPart2 :: Examples
examplesPart2 = [(")", 1), ("()())", 5)]

buildTestCases :: (String -> Int) -> Examples -> [TestTree]
buildTestCases f =
  map
    ( \(pattern, result) ->
        testCase (pattern ++ " should equal " ++ (show result)) (result @?= f pattern)
    )
