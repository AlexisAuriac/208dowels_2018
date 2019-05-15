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

displayPacksValues :: [Int] -> IO ()
displayPacksValues values = do
    putStr "\tOx\t| "
    displayPacksValues' values
    putStrLn "100"

displayPacksValues' :: [Int] -> IO ()
displayPacksValues' [] = return ()
displayPacksValues' (x:xs) = do
    printf "%d\t| " x
    displayPacksValues' xs

displayTheoricalSizes :: [Float] -> IO ()
displayTheoricalSizes packs = do
    putStr "\tTx\t| "
    displayTheoricalSizes' packs
    putStrLn "100"

displayTheoricalSizes' :: [Float] -> IO ()
displayTheoricalSizes' [] = return ()
displayTheoricalSizes' (x:xs) = do
    printf "%.1f\t| " x
    displayTheoricalSizes' xs

displayClasses :: [[Int]] -> [Float] -> IO ()
displayClasses packs thSizes = do
    displayPacks packs
    displayPacksValues (map sum packs)
    displayTheoricalSizes thSizes
