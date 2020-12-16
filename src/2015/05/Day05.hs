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

hasAnyTwoLettersTwice :: String -> Bool
hasAnyTwoLettersTwice [] = False
hasAnyTwoLettersTwice (x : y : xs) = ([x, y] `isInfixOf` xs) || hasAnyTwoLettersTwice (y : xs)
hasAnyTwoLettersTwice _ = False

hasMirroredLetter :: String -> Bool
hasMirroredLetter [] = False
hasMirroredLetter (x : y : z : rest) = x == z || hasMirroredLetter (y : z : rest)
hasMirroredLetter _ = False

isGoodStringPart2 :: String -> Bool
isGoodStringPart2 xs = hasAnyTwoLettersTwice xs && hasMirroredLetter xs

part2 :: String -> Int
part2 input = length $ filter isGoodStringPart2 (lines input)

main :: IO ()
main = do
  input <- getContents
  print ("Part 1: " ++ show (part1 input))
  print ("Part 2: " ++ show (part2 input))
