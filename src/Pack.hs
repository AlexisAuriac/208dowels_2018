module Pack (
    packClasses
) where

import Utility

packClasses :: [Int] -> [[Int]]
packClasses classes = packClasses' boxed
    where boxed = map (\x -> [x]) classes

packClasses' :: [[Int]] -> [[Int]]
packClasses' packs
    | allSupMin = packs
    | otherwise = packClasses' newPacks
    where
        sumClasses = map (\xs -> sum xs) packs
        supMin = map (> 9) sumClasses
        allSupMin = foldr (\x y -> x && y) True supMin
        (smallestIdx, _) = findSmallestPack sumClasses
        newPacks = mergeSmallestWithNeighbor packs smallestIdx

findSmallestPack :: [Int] -> (Int, Int)
findSmallestPack [] = error "Need at least one pack"
findSmallestPack (p:packs) = findSmallestPack' packs 0 p 1

findSmallestPack' :: [Int] -> Int -> Int -> Int -> (Int, Int)
findSmallestPack' [] bestIdx bestVal _ = (bestIdx, bestVal)
findSmallestPack' (p:packs) bestIdx bestVal curIdx
    | p < bestVal = findSmallestPack' packs curIdx p (curIdx+1)
    | otherwise = findSmallestPack' packs bestIdx bestVal (curIdx+1)

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
