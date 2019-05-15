module ChiSquared (
    chiSquared
) where

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
