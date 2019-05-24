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
import ChiSquared
import DisplayClasses
import FitValidity

displayDistribution :: Float -> IO ()
displayDistribution p = printf "Distribution:\t\tB(%d, %.4f)\n" (100 :: Int) p

displayChiSquared :: Float -> IO ()
displayChiSquared x2 = printf "Chi-squared:\t\t%.3f\n" x2

displayDegreesFreedom :: Int -> IO ()
displayDegreesFreedom freedom = printf "Degrees of freedom:\t%d\n" freedom

displayDowels :: ([[Int]], [Float], Float, Float, Int, (Int, Int)) -> IO ()
displayDowels (classPacks, thSizes, p, x2, freedom, fitValidity) = do
    displayClasses classPacks thSizes
    displayDistribution p
    displayChiSquared x2
    displayDegreesFreedom freedom
    displayFitValidity fitValidity

getValuesToDisplay :: [Int] -> ([[Int]], [Float], Float, Float, Int, (Int, Int))
getValuesToDisplay classes = (classPacks, thSizes, p, x2, freedom, fitValidity)
    where
        classPacks = packClasses classes
        classValues = map sum classPacks
        p = calcP classes (nbSamples * nbPiecesPerSample)
        thSizes = packsTheoricalSizes p classPacks
        x2 = chiSquared classValues thSizes
        freedom = (length classPacks) - 2
        fitValidity = getFitValidity x2 freedom

dowels :: IO ()
dowels = do
    observedClasses <- processArgv
    displayDowels $ getValuesToDisplay observedClasses

main :: IO ()
main = do
    res <- try dowels :: IO (Either SomeException ())
    case res of
        Left err -> do
            print err
            exitWith (ExitFailure 84)
        Right _ -> exitSuccess
