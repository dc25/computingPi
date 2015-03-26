import System.Environment
import Pi

-- compute pi to number of digits requested on cmd line
-- then count correct digits by comparing to precomputed digits
main :: IO ()
main = do
    args <- getArgs
    let digitsToCompute = read $ head args
    let computedDigits = piDigits digitsToCompute
    checkDigits <- getContents
    print $ length $ takeWhile (uncurry (==)) $ zip computedDigits checkDigits

