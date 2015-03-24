import System.Environment
import Data.Ratio

-- (exact formula thanks to wikipedia): pi/4 = 4 * atan 1/5 - atan 1/239
-- http://en.wikipedia.org/wiki/Machin-like_formula
pi' :: Int -> Rational
pi' digits = 4*(4 * atanTaylor (1%5) cutoff - atanTaylor (1%239) cutoff ) where
    cutoff = 1 % 10^digits
    atanTaylorTerms x = zipWith (/) (iterate ((-x*x)*) x) [1,3..] -- also from wikipedia
    atanTaylor x ct = sum $ takeWhile ((ct <).abs) $ atanTaylorTerms x

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

