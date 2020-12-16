module Day05 (part1, part2) where

import Data.List

hasSubstring :: String -> [String] -> Bool
hasSubstring xs = any (`isInfixOf` xs)

hasBadSubstring :: String -> Bool
hasBadSubstring xs = hasSubstring xs ["ab", "cd", "pq", "xy"]

hasDoubleLetter :: String -> Bool
hasDoubleLetter xs = hasSubstring xs doubles
  where
    doubles = map (\x -> x : [x]) ['a' .. 'z']

isVowel :: Char -> Bool
isVowel = flip elem "aeiou"

hasThreeVowels :: String -> Bool
hasThreeVowels = (>= 3) . length . filter isVowel

isGoodString :: String -> Bool
isGoodString xs = not (hasBadSubstring xs) && hasDoubleLetter xs && hasThreeVowels xs

part1 :: String -> Int
part1 input = length $ filter isGoodString (lines input)

part2 :: String -> Int
part2 input = 2

main :: IO ()
main = do
  input <- getContents
  print ("Part 1: " ++ show (part1 input))
  print ("Part 2: " ++ show (part2 input))
