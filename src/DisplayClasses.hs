module DisplayClasses (
    displayClasses
) where

import Text.Printf

import MathFunctions
import Constants

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

displayTheoricalSizes :: [[Int]] -> Float -> IO ()
displayTheoricalSizes packs p = do
    putStr "\tTx\t| "
    displayTheoricalSizes' (map length packs) p 0 0.0
    putStrLn "100"

displayTheoricalSizes' :: [Int] -> Float -> Int -> Float -> IO ()
displayTheoricalSizes' (x:[]) p idx t = do
    let tmp = map (\y -> theoricalSize y p nbSamples nbPiecesPerSample) [idx..100]
    printf "%.1f\t| " (sum tmp)
displayTheoricalSizes' (0:xs) p idx t = do
    printf "%.1f\t| " t
    displayTheoricalSizes' xs p idx 0.0
displayTheoricalSizes' (x:xs) p idx t = do
    displayTheoricalSizes' (x-1:xs) p (idx+1) (t + (theoricalSize idx p nbSamples nbPiecesPerSample))

displayClasses :: [[Int]] -> Float -> IO ()
displayClasses packs p = do
    displayPacks packs
    displayPacksSums packs
    displayTheoricalSizes packs p
