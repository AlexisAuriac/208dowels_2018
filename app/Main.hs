module Main where

import System.Exit
import Control.Exception
import Text.Printf

import Utility
import ProcessArgs
import MathFunctions
import Pack
import DisplayClasses
import Constants

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
