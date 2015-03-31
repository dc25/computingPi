module Pi (
    piDigits
) where

-- Take the terms up to a limit of the Taylor series of 
-- atan x, add them up to get atan x. 
--
-- Now scaling all operations by 10^digits to eliminate need to use
-- Rationals.  This also makes things lots faster.
-- Thanks to: http://en.literateprograms.org/Pi_with_Machin%27s_formula_(Haskell)
-- for this technique.
atan' :: Integer -> Int -> Integer
atan' x digits = sum $ takeWhile ((>0).abs) atanTerms where 
    atanTerms = zipWith div (iterate (`div`(-x*x)) (10^digits `div` x)) [1,3..] 

-- Exact formulas thanks to wikipedia: http://en.wikipedia.org/wiki/Machin-like_formula
-- pi/4 = 4 * atan 1/5 - atan 1/239
-- or (faster):
-- pi/4 = 183 * atan 1/239 + 32 * atan 1/1023 - 68 * atan 1/5832 + 12 * atan 1/110443 - 12 * atan 1/4841182 - 100 * atan 1/6826318
pi' :: Int -> Integer
pi' digits = 4*sum [scale * atan' x digits | (scale, x) <- parameters] where 
    parameters = [ (183, 239), (32, 1023), (-68, 5832), (12, 110443), (-12, 4841182), (-100, 6826318) ]

-- compute digits by adding zeros to numerator and dividing
piDigits :: Int -> String
piDigits digits = withDecimal where
    digitsOnly = show $ pi' digits
    withDecimal = head digitsOnly : "." ++ tail digitsOnly
