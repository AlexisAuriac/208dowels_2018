module ProcessArgs (processArgv) where

import System.Environment
import Control.Exception

import Utility

usage :: String -> String
usage progName = "Usage\n\
    \\t" ++ progName ++ " O0 O1 O2 O3 O4 O5 O6 O7 O8\n\
    \\n\
    \DESCRIPTION\n\
    \\tOi\tsize of the observed class\n"

processArgv :: IO [Int]
processArgv = do
    argv <- getArgs
    progName <- getProgName
    return $ processArgv' argv progName
processArgv' :: [String] -> String -> [Int]
processArgv' argv progName
    | length argv /= 9 = error $ usage progName
    | not $ allInts = error "All arguments must be integers"
    | sum converted /= 100 = error "The sum of all the observed classes must be 100"
    | otherwise = converted
    where
        resTestInts = map isInt argv
        allInts = foldr (\x y -> x && y) True resTestInts
        converted = map (\x -> read x :: Int) argv
