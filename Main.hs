import System.Environment
import Data.Ratio

atanTaylorTerms :: Rational -> [Rational]
atanTaylorTerms x = zipWith (/) (iterate ((-x*x)*) x) [1,3..] -- from wikipedia

atanTaylor :: Rational -> Rational -> Rational
atanTaylor x termLimit = sum $ takeWhile ((termLimit <).abs) $ atanTaylorTerms x

-- (exact formula thanks to wikipedia): pi/4 = 4 * atan 1/5 - atan 1/239
-- http://en.wikipedia.org/wiki/Machin-like_formula
pi' :: Int -> Rational
pi' digits = 4*sum [scale * atanTaylor arg termLimit | (scale, arg) <- parameters, let termLimit = (1%10^digits)/abs scale] where 
    parameters = [ ( 183, 1%239), (  32, 1%1023), ( -68, 1%5832), (  12, 1%110443), ( -12, 1%4841182), (-100, 1%6826318) ]

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

