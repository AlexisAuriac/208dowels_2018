module MathFunctions (
    binCoef,
    calcP,
    theoricalSize
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

theoricalSize :: Int -> Float -> Float -> Float -> Float
theoricalSize x p nbSamples sizeSmaple =
    nbSamples * coef * (p**xf) * ((1-p)**(nbSamples-xf))
    where
        xf = fromIntegral x :: Float
        xi = fromIntegral x :: Integer
        ni = round sizeSmaple
        coef = fromIntegral (binCoef ni xi) :: Float
