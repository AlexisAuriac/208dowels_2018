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

dowels :: IO ()
dowels = do
    observedClasses <- processArgv
    let classPacks = packClasses observedClasses
    let p = calcP observedClasses (nbSamples * nbPiecesPerSample)
    let thSizes = packsTheoricalSizes p classPacks
    displayClasses classPacks thSizes
    displayDistribution p

main :: IO ()
main = do
    res <- try dowels :: IO (Either SomeException ())
    case res of
        Left err -> do
            print err
            exitWith (ExitFailure 84)
        Right _ -> exitSuccess
