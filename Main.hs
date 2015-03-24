import Data.Ratio

-- compute atan x using n terms of taylor series
atanTaylor x n = sum $ take n $ zipWith (/) (iterate (*(x*x*(-1))) x) $ [1,3..]

-- pi/4 = 4 * atan 1/5 - atan 1/239
pi' = 4*(4 * atanTaylor (1%5) 2000 - atanTaylor (1%239) 2000)

-- compute digits by adding zeros to numerator and dividing
pi_digits = 10^(length $ show $ denominator pi') * numerator pi' `div` denominator pi'

main = do
        checkDigits <- getContents
        print $ length $ takeWhile (uncurry (==)) $ zip (show pi_digits) checkDigits

