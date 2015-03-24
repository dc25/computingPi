import System.Environment
import Data.Ratio

-- compute atan x using terms of taylor series greater than threshold
atanTaylor :: Rational -> Rational -> Rational
atanTaylor x threshold = sum terms where
    terms = takeWhile ((threshold <).abs) $ zipWith (/) (iterate (*negXSqrd) x) [1,3..] where
        negXSqrd = -x*x -- improves readabilty but had no effect on performance

-- (exact formula thanks to wikipedia): pi/4 = 4 * atan 1/5 - atan 1/239
-- http://en.wikipedia.org/wiki/Machin-like_formula
pi' :: Int -> Rational
pi' digits = 4*(4 * atanTaylor (1%5) threshold - atanTaylor (1%239) threshold) where
    threshold = 1 % 10^digits

-- compute digits by adding zeros to numerator and dividing
piDigits :: Int -> String
piDigits digits = show $ 10^length (show piDen) * piNum `div` piDen where
    computedPi = pi' digits
    piDen = denominator computedPi
    piNum = numerator computedPi

-- compute pi to number of digits requested on cmd line
-- then count correct digits by comparing to precomputed digits
main :: IO ()
main = do
    args <- getArgs
    let digitsToCompute = read $ head args::Int
    let computedDigits = piDigits digitsToCompute
    checkDigits <- getContents
    print $ length $ takeWhile (uncurry (==)) $ zip computedDigits checkDigits

