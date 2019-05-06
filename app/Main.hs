module Main where

import System.Exit
import Control.Exception

import Utility
import ProcessArgs

dowels :: IO ()
dowels = do
    observedClasses <- processArgv
    printList observedClasses print

main :: IO ()
main = do
    res <- try dowels :: IO (Either SomeException ())
    case res of
        Left err -> do
            print err
            exitWith (ExitFailure 84)
        Right _ -> exitSuccess
