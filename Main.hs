module Main(main) where

import System.Console.GetOpt
import System.Environment(getArgs)
import System.Exit
import Text.Read
import Pi

-- compute pi to number of digits requested on cmd line
-- then count correct digits by comparing to precomputed digits
--
-- See the haskell.org tutorial for explanation of 
-- pattern matching using field labels below.
-- https://www.haskell.org/tutorial/moretypes.html
--
main :: IO ()
main = do
    Options { optDigits = digitCount, optCheck = maybeCheckFile } <- getOptions
    let computedDigits = piDigits digitCount
    case maybeCheckFile of
        Nothing -> putStrLn $ take digitCount computedDigits 
        Just f -> do
            checkDigits <- readFile f
            print $ length $ takeWhile (uncurry (==)) $ zip computedDigits checkDigits

-- Everything below here is for command line argument evaluation.

-- Thanks to Leif Frenzel and Tomasz Zielonka for outlining
-- the command line processing techniques used here.
-- Original material at:
-- http://leiffrenzel.de/papers/commandline-options-in-haskell.html
-- https://mail.haskell.org/pipermail/haskell/2004-January/013412.html


getOptions :: IO Options 
getOptions = do
    args <- getArgs
    let ( actions, _, _ ) = getOpt RequireOrder options args 
    foldl (>>=) (return defaultOptions) actions

data Options = Options {
    optDigits :: Int,
    optCheck :: Maybe String -- optional filename to check against
}

defaultOptions :: Options
defaultOptions = Options {
    optDigits = 50,
    optCheck = Nothing
}

options :: [OptDescr (Options->IO Options)]
options = [
    Option "V" ["version"] (NoArg showVersion) "show version number",
    Option "d" ["digits"]  (ReqArg setOptDigits "NUMBER") "set the number of digits to generate",
    Option "c" ["check"]  (ReqArg setOptCheck "FILE") "specify a file containing known digits of pi to check results"
    ]

showVersion :: Options -> IO Options
showVersion _ = do
    putStrLn "Pi digit generator 3.1"
    exitSuccess

-- See the language report for explanation of the use of field labels below.
-- https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-490003.15

-- Update options to contain the number of digits of pi desired.
setOptDigits :: String -> Options -> IO Options
setOptDigits cmdLineArg opt = 
    case readMaybe cmdLineArg  of
         Just x -> return opt { optDigits = x}
         Nothing -> do
             putStrLn ("Invalid value for digits:" ++ cmdLineArg)
             exitFailure

-- Update options to specify a file to compare output to.
setOptCheck :: String -> Options -> IO Options
setOptCheck cmdLineArg opt = 
    return opt { optCheck = Just cmdLineArg }

