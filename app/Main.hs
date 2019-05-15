module Main where

import System.Exit
import Control.Exception
import Text.Printf

import Utility
import ProcessArgs
import MathFunctions

nbSamples :: Float
nbSamples = 100.0

nbPiecesPerSample :: Float
nbPiecesPerSample = 100.0

packClasses :: [Int] -> [[Int]]
packClasses classes = packClasses' init
    where init = map (\x -> [x]) classes

findSmallestPack :: [Int] -> (Int, Int)
findSmallestPack [] = error "Need at least one pack"
findSmallestPack (p:packs) = findSmallestPack' packs 0 p 1

findSmallestPack' :: [Int] -> Int -> Int -> Int -> (Int, Int)
findSmallestPack' [] bestIdx bestVal _ = (bestIdx, bestVal)
findSmallestPack' (p:packs) bestIdx bestVal curIdx
    | p < bestVal = findSmallestPack' packs curIdx p (curIdx+1)
    | otherwise = findSmallestPack' packs bestIdx bestVal (curIdx+1)

mergeFirstTwo :: [[a]] -> [[a]]
mergeFirstTwo (x:y:xs) = (x ++ y) : xs

mergeLastTwo :: [[a]] -> [[a]]
mergeLastTwo xs = reverse $ (correctFirst : (tail insertedReverse))
    where
        insertedReverse = mergeFirstTwo (reverse xs)
        correctFirst = reverse $ head insertedReverse

mergePrevious :: [[a]] -> Int -> [[a]]
mergePrevious xs idx = start ++ [merged] ++ end
    where
        start = take (idx - 1) xs
        end = drop (idx + 1) xs
        merge1 = xs !! idx
        merge2 = xs !! (idx - 1)
        merged = merge2 ++ merge1

mergeSmallestWithNeighbor :: [[Int]] -> Int -> [[Int]]
mergeSmallestWithNeighbor packs idx
    | length packs == 1 = packs
    | idx == length packs - 1 = mergeLastTwo packs
    | idx == 0 = mergeFirstTwo packs
    | sum1 < sum2 = mergePrevious packs idx
    | otherwise = mergePrevious packs (idx + 1)
    where
        sum1 = sum (packs !! (idx - 1))
        sum2 = sum (packs !! (idx + 1))

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

displayPacks :: [[Int]] -> IO ()
displayPacks packs = do
    putStr "\tx\t| "
    displayPacks' packs 0
    putStrLn "Total"

displayPacks' :: [[Int]] -> Int -> IO ()
displayPacks' (p:[]) idx = printf "%d+\t| " idx
displayPacks' (p:packs) idx = do
    let sizePack = length p
    if sizePack > 1
        then printf "%d-%d\t| " idx (idx + sizePack - 1)
        else printf "%d\t| " idx
    displayPacks' packs (idx + sizePack)

displayPacksSums :: [[Int]] -> IO ()
displayPacksSums packs = do
    putStr "\t0x\t| "
    displayPacksSums' packs
    putStrLn "100"

displayPacksSums' :: [[Int]] -> IO ()
displayPacksSums' [] = return ()
displayPacksSums' (x:xs) = do
    printf "%d\t| " (sum x)
    displayPacksSums' xs

displayTheoricSizes :: [[Int]] -> Float -> IO ()
displayTheoricSizes packs p = do
    putStr "\tTx\t| "
    displayTheoricSizes' (map length packs) p 0 0.0
    putStrLn "100"

displayTheoricSizes' :: [Int] -> Float -> Int -> Float -> IO ()
displayTheoricSizes' (x:[]) p idx t = do
    let tmp = map (\y -> theoricSize y p nbSamples nbPiecesPerSample) [idx..100]
    printf "%.1f\t| " (sum tmp)
displayTheoricSizes' (0:xs) p idx t = do
    printf "%.1f\t| " t
    displayTheoricSizes' xs p idx 0.0
displayTheoricSizes' (x:xs) p idx t = do
    displayTheoricSizes' (x-1:xs) p (idx+1) (t + (theoricSize idx p nbSamples nbPiecesPerSample))

displayClasses :: [[Int]] -> Float -> IO ()
displayClasses packs p = do
    displayPacks packs
    displayPacksSums packs
    displayTheoricSizes packs p

displayDistribution :: Float -> IO ()
displayDistribution p = printf "Distribution:\t\tB(%d, %.4f)\n" (100 :: Int) p

dowels :: IO ()
dowels = do
    observedClasses <- processArgv
    let classPacks = packClasses observedClasses
    let p = calcP observedClasses (nbSamples * nbPiecesPerSample)
    displayClasses classPacks p
    displayDistribution p

main :: IO ()
main = do
    res <- try dowels :: IO (Either SomeException ())
    case res of
        Left err -> do
            print err
            exitWith (ExitFailure 84)
        Right _ -> exitSuccess
