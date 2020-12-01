module Day04 (part1, part2) where

import Data.ByteString.Char8 (pack)
import Data.ByteString.Lazy (fromStrict)
import Data.Digest.Pure.MD5 (MD5Digest, md5)
import Data.List

getMD5 :: String -> MD5Digest
getMD5 = md5 . fromStrict . pack

buildKey :: String -> Int -> String
buildKey key number = key ++ show number

getFinal :: String -> Int -> String -> String -> Int
getFinal hash number prefix key
  | prefix `isPrefixOf` hash = number
  | otherwise = getFinal nextHash nextNumber prefix key
  where
    nextNumber = number + 1
    nextHash = show $ getMD5 $ buildKey key nextNumber

part1 :: String -> Int
part1 input = getFinal "" 1 "00000" $ head $ lines input

part2 :: String -> Int
part2 input = getFinal "" 1 "000000" $ head $ lines input

main :: IO ()
main = do
  input <- getContents
  print ("Part 1: " ++ show (part1 input))
  print ("Part 2: " ++ show (part2 input))
