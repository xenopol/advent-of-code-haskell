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
hasAnyTwoLettersTwice xs = test xs False
  where
    test _ True = True
    test [] _ = False
    test [_] _ = False
    test (x:y:rest) _ = test (y : rest) ((x : y : []) `isInfixOf` rest)

hasRepeatingLetter :: String -> Bool
hasRepeatingLetter xs = test xs False
  where
    test _ True = True
    test [] _ = False
    test [_] _ = False
    test [_, _] _ = False
    test (x:z:y:rest) _ = test (z : y : rest) (x == y)

isGoodStringPart2 :: String -> Bool
isGoodStringPart2 xs = hasAnyTwoLettersTwice xs && hasRepeatingLetter xs

part2 :: String -> Int
part2 input = length $ filter isGoodStringPart2 (lines input)

main :: IO ()
main = do
  input <- getContents
  print ("Part 1: " ++ show (part1 input))
  print ("Part 2: " ++ show (part2 input))
