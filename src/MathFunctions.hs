module MathFunctions (
    binCoef,
    calcP,
    theoricSize
) where

binCoef :: Integer -> Integer -> Integer
binCoef _ 0 = 1
binCoef 0 _ = 0
binCoef n k = (binCoef (n-1) (k-1)) * (n `div` k)

calcP :: [Int] -> Float -> Float
calcP classes n = calcP' (map toFloat classes) n 0.0 0.0
    where
        toFloat i = fromIntegral i :: Float

calcP' :: [Float] -> Float -> Float -> Float -> Float
calcP' [] n _ res = res / n
calcP' (val:values) n x res = calcP' values n (x+1) (res + val*x)

theoricSize :: Int -> Float -> Float -> Float -> Float
theoricSize x p nbSamples sizeSmaple =
    nbSamples * coef * (p**xf) * ((1-p)**(nbSamples-xf))
    where
        xf = fromIntegral x :: Float
        xi = fromIntegral x :: Integer
        -- nf = 100.0 :: Float
        ni = 100 :: Integer
        coef = fromIntegral (binCoef ni xi) :: Float
