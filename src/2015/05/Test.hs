import Day05
import Test.Tasty
import Test.Tasty.HUnit

main = do
  defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Day 5"
    [ testGroup "Part 1" $ buildTestCases part1 examplesPart1
    -- testGroup "Part 2" $ buildTestCases part2 examplesPart2
    ]

type Examples = [([Char], Int)]

examplesPart1 :: Examples
examplesPart1 =
  [ ("ugknbfddgicrmopn", 1),
    ("aaa", 1),
    ("jchzalrnumimnmhp", 0),
    ("haegwjzuvuyypxyu", 0),
    ("dvszwmarrgswjxmb", 0)
  ]

examplesPart2 :: Examples
examplesPart2 =
  [ ("qjhvhtzxzqqjkmpb", 1),
    ("xxyxx", 1),
    ("uurcxstgmygtbstg", 0),
    ("ieodomkazucvgmuy", 0)
  ]

buildTestCases :: (String -> Int) -> Examples -> [TestTree]
buildTestCases f =
  map
    ( \(pattern, result) ->
        testCase (pattern ++ " should equal " ++ (show result)) (f pattern @?= result)
    )
