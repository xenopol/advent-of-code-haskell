module Day03 (part1, part2) where

import Data.List

type Move = (Int, Int)

parseMove :: Move -> Char -> Move
parseMove (x, y) '^' = (x + 1, y)
parseMove (x, y) 'v' = (x - 1, y)
parseMove (x, y) '>' = (x, y + 1)
parseMove (x, y) '<' = (x, y - 1)

uniqueMoves :: String -> [Move]
uniqueMoves = nub . scanl parseMove (0, 0)

filterString :: (Int -> Bool) -> String -> String
filterString f input =
  map snd $ filter (f . fst) movesWithIndexes
  where
    movesWithIndexes = zip [0 ..] input

santaMoves :: String -> [Move]
santaMoves = uniqueMoves . filterString even

robotSantaMoves :: String -> [Move]
robotSantaMoves = uniqueMoves . filterString odd

part1 :: String -> Int
part1 input = sum $ map (length . uniqueMoves) (lines input)

part2 :: String -> Int
part2 input =
  sum $ map (length . moves) (lines input)
  where
    moves line = santaMoves line `union` robotSantaMoves line

main :: IO ()
main = do
  input <- getContents
  print ("Part 1: " ++ show (part1 input))
  print ("Part 2: " ++ show (part2 input))
