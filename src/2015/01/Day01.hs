module Day01 (part1, part2) where

getNumbers :: String -> [Int]
getNumbers = map (\c -> if c == '(' then 1 else -1)

part1 :: String -> Int
part1 = sum . getNumbers

part2 :: String -> Int
part2 =
  (+ 1)
    . length
    . takeWhile (/= -1)
    . scanl1 (+)
    . getNumbers

main = do
  input <- getLine
  print ("Part 1: " ++ show (part1 input))
  print ("Part 2: " ++ show (part2 input))
