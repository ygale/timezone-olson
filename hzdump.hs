-- | A clone in Haskell of the zdump(8) command, including most of
-- the bugs, that takes paths to Olson files instead of timezone
-- names. This is useful for testing the Haskell implementation
-- of the Olson timezone parser and renderer against the reference
-- implementation in C. But less useful than it might seem at first,
-- because Haskell rounds historical solar mean timezones to the
-- nearest minute, whereas the C implementation rounds to the nearest
-- second.
module Main where

import Data.Time.LocalTime.TimeZone.Series
import Data.Time.LocalTime.TimeZone.Olson
import Data.Time
import Data.Maybe (listToMaybe)
import System.Environment (getArgs)
import System.Exit (exitWith, exitSuccess, ExitCode(ExitFailure))
import System.Locale (defaultTimeLocale)

version :: IO a
version = do
  putStrLn "hzdump version 0.1, clone of zdump 1.7  -c option fixed,"
  putStrLn "takes zone file paths instead of zone specifications"
  exitSuccess

usage :: IO a
usage = do
  putStrLn "usage: hzdump [--version] [-v] [-c cutoff] zone-file-path ..."
  putStrLn "  where cutoff is lo-year,hi-year or hi-year"
  exitWith $ ExitFailure 1

illegalOpt :: Char -> IO a -- sic
illegalOpt opt = do
  putStrLn $ "hzdump: illegal option -- " ++ [opt]
  usage

data Option = Version | Illegal Char | Verbose (Maybe Integer) | Now
  deriving (Eq, Ord, Show)

main = do
    (opts, zones) <- fmap parseArgs getArgs
    (getTimes, displayTime) <- case opts of
      Version     -> version
      Illegal opt -> illegalOpt opt
      Verbose yr  -> return (transitionTimesUntil yr, displayVerbose)
      _           -> do now <- getCurrentTime
                        return (const [now], displayConcise)
    tzss <- mapM getZone zones
    putStr . unlines . concat $
      zipWith (displayZone getTimes displayTime) zones tzss
  where
    getZone "-" = return utcTZ -- sic
    getZone z   = getTimeZoneSeriesFromOlsonFile z

    displayZone getTimes displayTime zone tzs =
      map ((zone ++) . ("  " ++) . displayTime tzs) $ getTimes tzs

parseArgs :: [String] -> (Option, [FilePath])
parseArgs args | "--version" `elem` args = (Version, [])
parseArgs args = getOpts False Nothing args
  where
    getOpts _ cutoff  ("-v":args)   = getOpts True cutoff args
    getOpts v Nothing ("-c":c:args) = maybe (Illegal 'c', [])
                                      (\y -> getOpts v (Just y) args) $
                                      maybeRead c
    getOpts v cutoff  ("--":zones ) = (opts v cutoff, zones)
    getOpts _ _       (('-':x:_):_) = (Illegal x, [])
    getOpts v cutoff  zones         = (opts v cutoff, zones)
    opts v cutoff = if v then Verbose cutoff else Now

displayConcise :: TimeZoneSeries -> UTCTime -> String
displayConcise tzs t = formatTime defaultTimeLocale format $
                       ZoneSeriesTime t tzs
  where
    format = "%a %b %e %T %Y %Z"

displayVerbose :: TimeZoneSeries -> UTCTime -> String
displayVerbose tzs t = concat [displayConcise utcTZ t, " = ",
    displayConcise tzs t, " isdst=", if isdst then "1" else "0"]
  where
    isdst = timeZoneSummerOnly $ timeZoneFromSeries tzs t

transitionTimesUntil :: Maybe Integer -> TimeZoneSeries -> [UTCTime]
transitionTimesUntil yr =
    maybe id (dropWhile . (>=)) (fmap happyNewYear yr) .
    addBugs . addPrevSecond . actualTransitions
  where
    happyNewYear y = UTCTime (fromGregorian y 1 1) 0
    actualTransitions (TimeZoneSeries d cs) =
      let rs = reverse cs
      in map snd . filter fst $
         zipWith (\(t, tz) prevTz -> (tz /= prevTz, t))
                 rs
                 (d : map snd rs)
    addPrevSecond = concatMap (\t -> [(-1) `addUTCTime` t, t])
    addBugs = (bug0 ++) . (++ bug1) -- sic
    bug0 = [UTCTime (fromGregorian 1901 12 13) (20*3600+45*60+52),
            UTCTime (fromGregorian 1901 12 14) (20*3600+45*60+52)]
    bug1 = [UTCTime (fromGregorian 2038  1 18) ( 3*3600+14*60+ 7),
            UTCTime (fromGregorian 2038  1 19) ( 3*3600+14*60+ 7)]

utcTZ :: TimeZoneSeries
utcTZ = TimeZoneSeries utc []

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . filter (null . snd) . reads