module Utility (
    allElemsOf,
    isUint,
    isInt,
    printList
) where

allElemsOf :: (Eq a) => [a] -> [a] -> Bool
allElemsOf [] _ = True
allElemsOf (c:xs) src = c `elem` src && (allElemsOf xs src)

isUint :: String -> Bool
isUint "" = False
isUint xs = allElemsOf xs "0123456789"

isInt :: String -> Bool
isInt "" = False
isInt (x:xs)
    | x == '-' = allElemsOf xs digits
    | otherwise = allElemsOf (x:xs) digits
    where digits = "0123456789"

printList :: (Show a) => [a] -> (a -> IO ()) -> IO ()
printList [] _ = return ()
printList (x:xs) f = do
    f x
    printList xs f
