module Main where

import Data.Time
import System.Exit (exitWith, exitSuccess, ExitCode(..))
import System.Process (readProcess)
import Data.List (isInfixOf)
import Data.Char (toLower)

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
isBeforeMidnightCheck ::  (String -> IO TimeZone) ->IO Bool
isBeforeMidnightCheck loadSystemTimeZone = do
    -- Get the current time in UTC
    now <- getCurrentTime

    -- Get New York's timezone (America/New_York)
    nyTZ <- loadSystemTimeZone "America/New_York"

    -- Get local timezone (which is east of NY in our case)
    localTZ <- getCurrentTimeZone

    -- Convert current UTC time to local time
    let localT = utcToLocalTime localTZ now
        localTD = localTimeOfDay localT
        localHour = todHour localTD

    -- Convert the same UTC time to NY time for reference
    let nyTime = utcToLocalTime nyTZ now
        nyDay = localDay nyTime

    -- Calculate tomorrow and yesterday in NY timezone
    let tomorrow = addDays 1 nyDay
        yesterday = addDays (-1) nyDay

    -- Since we're east of NY and before noon local time,
    -- check if we're in yesterday's or tomorrow's boundary
    if localHour < 12
        then do
            -- Calculate the UTC midnight points for comparison
            tomorrowMidnight <- timeToTimeZone tomorrow nyTZ
            yesterdayMidnight <- timeToTimeZone yesterday nyTZ
            return $ now < tomorrowMidnight && now > yesterdayMidnight
        else
            return False
    where
        -- convert local midnight to UTC
        timeToTimeZone day tz = do
            let midnite = LocalTime day (TimeOfDay 0 0 0)
            return $ localTimeToUTC tz midnite


-- Check if any file contains the forbidden word
containsForbiddenWord :: String -> [String] -> Bool
containsForbiddenWord word = any (isInfixOf (map toLower word) . map toLower)

main :: IO ()
main = do
    -- Check if any files in the repository contain forbidden day numbers
    currentDay <- (\(_, _, d) -> d) . toGregorian . utctDay <$> getCurrentTime
    let forbiddenWords = [numberToWord currentDay]

    -- Get all tracked files
    files <- lines <$> readProcess "git" ["diff", "@{push}"] "--name-only"

    let hasProblematicNames = any (`containsForbiddenWord` files) forbiddenWords

    if hasProblematicNames
        then do
            putStrLn "Error: Repository contains files with current day number in text form."
            putStrLn $ "Forbidden words: " ++ show forbiddenWords
            exitWith (ExitFailure 1)
        else exitSuccess
