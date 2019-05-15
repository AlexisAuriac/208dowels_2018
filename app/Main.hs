module Main where

import System.Exit
import Control.Exception
import Text.Printf

import Utility
import ProcessArgs
import MathFunctions
import Pack

nbSamples :: Float
nbSamples = 100.0

nbPiecesPerSample :: Float
nbPiecesPerSample = 100.0

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
