module Pack (
    packClasses
) where

import Utility

packClasses :: [Int] -> [[Int]]
packClasses classes = packClasses' boxed
    where boxed = [[i] | i <- classes]

packClasses' :: [[Int]] -> [[Int]]
packClasses' packs
    | allSupMin = packs
    | otherwise = packClasses' newPacks
    where
        sumClasses = map sum packs
        supMin = map (> 9) sumClasses
        allSupMin = foldr (&&) True supMin
        (smallestIdx, _) = findBest sumClasses (<)
        newPacks = mergeSmallestWithNeighbor packs smallestIdx

mergeSmallestWithNeighbor :: [[Int]] -> Int -> [[Int]]
mergeSmallestWithNeighbor packs idx
    | length packs == 1 = packs
    | idx == length packs - 1 = myMergeLast
    | idx == 0 = myMergeFirst
    | prev < next = myMergePrevious idx
    | otherwise = myMergePrevious (idx + 1)
    where
        myMergeLast = mergeLastTwo packs (++)
        myMergeFirst = mergeFirstTwo packs (++)
        myMergePrevious = mergePrevious packs (++)
        prev = sum (packs !! (idx - 1))
        next = sum (packs !! (idx + 1))
