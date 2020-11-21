module Day02 (part1, part2) where

import Data.List.Split

type Dimensions = (Int, Int, Int)

type Side = (Int, Int)

tuplify3 :: [Int] -> Dimensions
tuplify3 [] = (0, 0, 0)
tuplify3 [x] = (x, 0, 0)
tuplify3 [x, y] = (x, y, 0)
tuplify3 (x : y : z : _) = (x, y, z)

getDimensions :: String -> Dimensions
getDimensions = tuplify3 . map read . splitOn "x"

getSmallestSide :: Dimensions -> Side
getSmallestSide (l, w, h)
  | l >= w && l >= h = (w, h)
  | h >= l && h >= w = (l, w)
  | otherwise = (h, l)

getSmallestSideArea :: Dimensions -> Int
getSmallestSideArea =
  uncurry (*) . getSmallestSide

getSmallestSidePerimeter :: Dimensions -> Int
getSmallestSidePerimeter = (* 2) . uncurry (+) . getSmallestSide

getSurfaceArea :: Dimensions -> Int
getSurfaceArea (l, w, h) = (2 * l * w) + (2 * w * h) + (2 * h * l)

getVolume :: Dimensions -> Int
getVolume (l, w, h) = l * w * h

getWrappingPaperLength :: String -> Int
getWrappingPaperLength s =
  getSurfaceArea dimensions + getSmallestSideArea dimensions
  where
    dimensions = getDimensions s

getRibbonLength :: String -> Int
getRibbonLength s =
  getSmallestSidePerimeter dimensions + getVolume dimensions
  where
    dimensions = getDimensions s

part1 :: String -> Int
part1 input =
  sum $ map getWrappingPaperLength (lines input)

part2 :: String -> Int
part2 input =
  sum $ map getRibbonLength (lines input)

main :: IO ()
main = do
  input <- getContents
  print ("Part 1: " ++ show (part1 input))
  print ("Part 2: " ++ show (part2 input))
