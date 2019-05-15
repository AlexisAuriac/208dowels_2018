module Utility (
    allElemsOf,
    isUint,
    isInt,
    printList,
    average,
    mergeFirstTwo,
    mergeLastTwo,
    mergePrevious
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

average :: (Integral a) => [a] -> Float
average arr = sumArr / len
    where
        sumArr = fromIntegral (sum arr) :: Float
        len = fromIntegral (length arr) :: Float

mergeFirstTwo :: [a] -> (a -> a -> a) -> [a]
mergeFirstTwo (x:y:xs) merger = (x `merger` y) : xs

mergeLastTwo :: [a] -> (a -> a -> a) -> [a]
mergeLastTwo xs merger = start ++ merged
    where
        start = take ((length xs) - 2) xs
        last1 = last xs
        last2 = xs !! ((length xs) - 2)
        merged = [last1 `merger` last2]

mergePrevious :: [a] -> (a -> a -> a) -> Int -> [a]
mergePrevious xs merger idx = start ++ [merged] ++ end
    where
        start = take (idx - 1) xs
        end = drop (idx + 1) xs
        merge1 = xs !! idx
        merge2 = xs !! (idx - 1)
        merged = merge2 `merger` merge1
