module Main where

import System.Exit
import Control.Exception
import Text.Printf

import Utility
import ProcessArgs

nbSamples :: Float
nbSamples = 100.0

nbPiecesPerSample :: Float
nbPiecesPerSample = 100.0

calcP :: [Int] -> Float
calcP classes = calcP' (map toFloat classes) 0.0 0.0
    where
        toFloat i = fromIntegral i :: Float

calcP' :: [Float] -> Float -> Float -> Float
calcP' [] x res = res / (nbSamples * nbPiecesPerSample)
calcP' (val:values) x res = calcP' values (x+1) (res + val*x)

binCoef :: Integer -> Integer -> Integer
binCoef n 0 = 1
binCoef 0 k = 0
binCoef n k = binCoef (n-1) (k-1) * n `div` k

theoricSize :: Int -> Float -> Float
theoricSize x p = n1 * coef * (p**xf) * ((1-p)**(n1-xf))
    where
        xf = fromIntegral x :: Float
        xi = fromIntegral x :: Integer
        n1 = 100.0 :: Float
        n2 = 100 :: Integer
        coef = fromIntegral (binCoef n2 xi) :: Float

dowels :: IO ()
dowels = do
    observedClasses <- processArgv
    printList observedClasses print
    printf "Distribution:\t\tB(%d, %.4f)\n" (100 :: Int) (calcP observedClasses)

main :: IO ()
main = do
    res <- try dowels :: IO (Either SomeException ())
    case res of
        Left err -> do
            print err
            exitWith (ExitFailure 84)
        Right _ -> exitSuccess
