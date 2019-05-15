module PacksTheoricalSizes(
    packsTheoricalSizes
) where

import MathFunctions
import Constants

packsRanges :: [[Int]] -> [(Int, Int)]
packsRanges packs = packsRanges' packs 0 []

packsRanges' :: [[Int]] -> Int -> [(Int, Int)] -> [(Int, Int)]
packsRanges' (_:[]) idx res = reverse $ ((idx, 100) : res)
packsRanges' (p:packs) idx res = packsRanges' packs newIdx newRes
    where
        newIdx = idx + (length p)
        newRange = (idx, newIdx)
        newRes = newRange : res

packsTheoricalSizes :: Float -> [[Int]] -> [Float]
packsTheoricalSizes p packs = map (packTheoricalSize p) ranges
    where
        ranges = packsRanges packs

packTheoricalSize :: Float -> (Int, Int) -> Float
packTheoricalSize p (start, end) = packTheoricalSize' p start end 0.0

packTheoricalSize' :: Float -> Int -> Int -> Float -> Float
packTheoricalSize' p start end res
    | start == end = res
    | otherwise = packTheoricalSize' p (start+1) end (res + thSize)
    where
        thSize = theoricalSize start p nbSamples nbPiecesPerSample
