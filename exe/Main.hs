module Main where

import Data.Time
import Data.Time.Calendar.OrdinalDate
import System.Exit (exitWith, exitSuccess, ExitCode(..))
import System.Process (readProcess)
import Data.List (isInfixOf)
import Data.Char (toLower)

-- Convert a number to its English word representation (1-31)
numberToWord :: Int -> String
numberToWord n = case n of
    1 -> "one"
    2 -> "two"
    3 -> "three"
    4 -> "four"
    5 -> "five"
    6 -> "six"
    7 -> "seven"
    8 -> "eight"
    9 -> "nine"
    10 -> "ten"
    11 -> "eleven"
    12 -> "twelve"
    13 -> "thirteen"
    14 -> "fourteen"
    15 -> "fifteen"
    16 -> "sixteen"
    17 -> "seventeen"
    18 -> "eighteen"
    19 -> "nineteen"
    20 -> "twenty"
    21 -> "twentyon"
    22 -> "twentytwo"
    23 -> "twentythree"
    24 -> "twentyfour"
    25 -> "twentyfive"
    26 -> "twentysix"
    27 -> "twentyseven"
    28 -> "twentyeight"
    29 -> "twentynine"
    30 -> "thirty"
    31 -> "thirtyone"
    _ -> error "Invalid day number"

-- Get the day number we should check for, considering the time
getRelevantDay :: IO Int
getRelevantDay = do
    now <- getZonedTime
    let localTime = zonedTimeToLocalTime now
        (_, monthDay) = toOrdinalDate (localDay localTime)
        hour = todHour $ localTimeOfDay localTime

    return $ if hour < 6
            then if monthDay == 1
                then 31  -- Handle month boundary
                else monthDay - 1
            else monthDay

-- Check if any file contains the forbidden word
containsForbiddenWord :: String -> [String] -> Bool
containsForbiddenWord word = any (isInfixOf (map toLower word) . map toLower)

main :: IO ()
main = do
    -- Check if any files in the repository contain forbidden day numbers
    currentDay <- getRelevantDay
    let forbiddenWords = [numberToWord currentDay]

    -- Get all tracked files
    files <- lines <$> readProcess "git" ["ls-files"] ""

    let hasProblematicNames = any (`containsForbiddenWord` files) forbiddenWords

    if hasProblematicNames
        then do
            putStrLn "Error: Repository contains files with current day number in text form."
            putStrLn $ "Forbidden words: " ++ show forbiddenWords
            exitWith (ExitFailure 1)
        else exitSuccess