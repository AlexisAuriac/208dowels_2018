module Main where

import System.Exit
import Control.Exception
import Text.Printf

import Utility
import ProcessArgs
import MathFunctions
import Constants
import Pack
import PacksTheoricalSizes
import DisplayClasses

displayDistribution :: Float -> IO ()
displayDistribution p = printf "Distribution:\t\tB(%d, %.4f)\n" (100 :: Int) p

displayChiSquared :: Float -> IO ()
displayChiSquared x2 = printf "Chi-squared:\t\t%.3f\n" x2

chiSquared :: [Int] -> [Float] -> Float
chiSquared values thSizes = chiSquared' fValues thSizes 0.0
    where fValues = map (\x -> fromIntegral x :: Float) values

chiSquared' :: [Float] -> [Float] -> Float -> Float
chiSquared' [] [] res = res
chiSquared' (ox:values) (tx:thSizes) res = chiSquared' values thSizes (res+y)
    where
        numerator = (ox - tx) ** 2
        denominator = tx
        y = numerator / denominator

dowels :: IO ()
dowels = do
    observedClasses <- processArgv
    let classPacks = packClasses observedClasses
    let classValues = map sum classPacks
    let p = calcP observedClasses (nbSamples * nbPiecesPerSample)
    let thSizes = packsTheoricalSizes p classPacks
    let x2 = chiSquared classValues thSizes
    displayClasses classPacks thSizes
    displayDistribution p
    displayChiSquared x2

main :: IO ()
main = do
    res <- try dowels :: IO (Either SomeException ())
    case res of
        Left err -> do
            print err
            exitWith (ExitFailure 84)
        Right _ -> exitSuccess
