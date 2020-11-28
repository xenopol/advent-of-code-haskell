import Day03
import Test.Tasty
import Test.Tasty.HUnit

main = do
  defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Day 3"
    [ testGroup "Part 1" $ buildTestCases part1 examplesPart1,
      testGroup "Part 2" $ buildTestCases part2 examplesPart2
    ]

type Examples = [([Char], Int)]

examplesPart1 :: Examples
examplesPart1 =
  [ (">", 2),
    ("^>v<", 4),
    ("^v^v^v^v^v", 2)
  ]

examplesPart2 :: Examples
examplesPart2 =
  [ ("^v", 3),
    ("^>v<", 3),
    ("^v^v^v^v^v", 11)
  ]

buildTestCases :: (String -> Int) -> Examples -> [TestTree]
buildTestCases f =
  map
    ( \(pattern, result) ->
        testCase (pattern ++ " should equal " ++ (show result)) (f pattern @?= result)
    )
