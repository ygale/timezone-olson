{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Time.LocalTime.TimeZone.Olson.Parse
-- Copyright   :  Yitzchak Gale 2010
--
-- Maintainer  :  Yitzchak Gale <gale@sefer.org>
-- Portability :  portable
--
-- A parser for binary Olson timezone files whose format are specified
-- by the tzfile(5) man page on Unix-like systems. For more
-- information about this format, see
-- http://www.twinsun.com/tz/tz-link.htm. Functions are provided for
-- converting the parsed data into @TimeZoneSeries@ and @TimeZone@
-- objects.

{-
Copyright (c) 2010 Yitzchak Gale. All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Yitzchak Gale nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
----------------------------------------------------------------------------}

module Data.Time.LocalTime.TimeZone.Olson.Parse
(
 getTimeZoneSeriesFromOlsonFile,
 getOlsonFromFile,
 olsonToTimeZoneSeries,
 getOlson,
 OlsonError
)
where

import Data.Time.LocalTime.TimeZone.Olson.Types
import Data.Time.LocalTime.TimeZone.Series (TimeZoneSeries(..))
import Data.Time (TimeZone(TimeZone))
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Binary.Get (Get, runGet, getWord8, getWord32be, getWord64be,
                        getByteString, getRemainingLazyByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Monoid (mappend)
import Data.Maybe (listToMaybe, fromMaybe)
import Data.Word (Word8)
import Data.Int (Int32, Int64)
import Data.Typeable (Typeable)
import Control.Monad (guard, replicateM, replicateM_)
import Control.Exception.Extensible (try, throw, Exception, ErrorCall)

-- | An exception indicating that the binary data being parsed was not
-- valid Olson timezone data
data OlsonError = OlsonError String
  deriving Typeable
instance Show OlsonError where
  show (OlsonError msg) = msg
instance Exception OlsonError

-- | Convert parsed Olson timezone data into a @TimeZoneSeries@.
olsonToTimeZoneSeries :: OlsonData -> Maybe TimeZoneSeries
olsonToTimeZoneSeries (OlsonData ttimes ttinfos@(dflt0:_) _ _) =
    fmap (TimeZoneSeries $ mkTZ dflt) . mapM (lookupTZ ttinfos) $
    reverse ttimes
  where
    dflt = fromMaybe dflt0 . listToMaybe $ filter isStd ttinfos
    isStd (TtInfo _ isdst _ _) = not isdst
    mkTZ (TtInfo gmtoff isdst _ abbr) =
      TimeZone ((gmtoff + 30) `div` 60) isdst abbr
    lookupTZ ttinfos ttime = fmap (((,) $ toUTC ttime) . mkTZ) . listToMaybe $
                             drop (transIndex ttime) ttinfos
    toUTC = posixSecondsToUTCTime . fromIntegral . transTime
olsonToTimeZoneSeries _ = Nothing

-- | Read timezone data from a binary Olson timezone file and convert
-- it into a @TimeZoneSeries@.
getTimeZoneSeriesFromOlsonFile :: FilePath -> IO TimeZoneSeries
getTimeZoneSeriesFromOlsonFile fp = getOlsonFromFile fp >>=
  maybe (throwOlson fp "no timezone found in OlsonData") return .
    olsonToTimeZoneSeries

-- | Parse a binary Olson timezone file.
getOlsonFromFile :: FilePath -> IO OlsonData
getOlsonFromFile fp = do
    e <- try . fmap (runGet getOlson) $ L.readFile fp
    either (formatError fp) return e

formatError :: FilePath -> ErrorCall -> IO a
formatError fp e = throwOlson fp $ show e

-- | A binary parser for binary Olson timezone files
getOlson :: Get OlsonData
getOlson = do
    getByteString 4
      >>= verify ((== "TZif") . toASCII . B.unpack) "missing magic number"
    version <- getWord8
      >>= verify (`elem` [0, 50]) "invalid version number"
    replicateM_ 15 getWord8 -- padding nulls
    getOlsonParts version

-- There is one part for Version 1 format data, and two parts and a POSIX
-- TZ string for Version 2 format data
getOlsonParts :: Word8 -> Get OlsonData
getOlsonParts 0  = getOlsonPart get32bitInteger
getOlsonParts 50 = do part1 <- getOlsonPart get32bitInteger
                      part2 <- getOlsonPart get64bitInteger
                      posixTZ <- getPosixTZ
                      return $ part1 `mappend` part2 `mappend` posixTZ
getOlsonParts _  = error "Invalid version number" -- never reached

-- Parse the part of an Olson file that contains timezone data
getOlsonPart :: Integral a => Get a -> Get OlsonData
getOlsonPart getTime = do
    tzh_ttisgmtcnt <- get32bitInt
    tzh_ttisstdcnt <- get32bitInt
    tzh_leapcnt <- get32bitInt
    tzh_timecnt <- get32bitInt
    tzh_typecnt <- get32bitInt
    tzh_charcnt <- get32bitInt
    times <- fmap (map toInteger) $ replicateM tzh_timecnt getTime
    indexes <- replicateM tzh_timecnt get8bitInt
    ttinfos <- replicateM tzh_typecnt getTtInfo
    abbr_chars <- fmap (toASCII . B.unpack) $ getByteString tzh_charcnt
    leaps <- replicateM tzh_leapcnt $ getLeapInfo getTime
    isstds <- replicateM tzh_ttisstdcnt getBool
    isgmts <- replicateM tzh_ttisgmtcnt getBool
    return $ OlsonData
      (zipWith Transition times indexes)
      (map (flip lookupAbbr abbr_chars) . zipWith setTtype ttinfos .
         (++ repeat UnknownType) $
         zipWith boolsToTType (map Just isstds ++ repeat Nothing) isgmts)
      leaps
      Nothing
  where
    lookupAbbr (TtInfo gmtoff isdst ttype abbrind) =
      TtInfo gmtoff isdst ttype . takeWhile (/= '\NUL') . drop abbrind
    setTtype ttinfo ttype = ttinfo {tt_ttype = ttype}
    boolsToTType _     isgmt | isgmt = UTC
    boolsToTType isstd _             = maybe UnknownType stdOrWall isstd
    stdOrWall isstd
     | isstd     = Std
     | otherwise = Wall

-- Parse a POSIX-style TZ string.
-- We don't try to understand the TZ string, we just pass it along whole.
getPosixTZ :: Get OlsonData
getPosixTZ = do
    getWord8 >>= verify (== 10)
                 "POSIX TZ string not preceded by newline"
    posixTZ <- fmap (L.takeWhile (/= 10)) getRemainingLazyByteString
    -- We don't check for the trailing newline, in order to keep it lazy
    return . OlsonData [] [] [] $ do
      guard (not $ L.null posixTZ)
      Just . toASCII $ L.unpack posixTZ

-- Parse a ttinfo struct. Each ttinfo struct corresponds to a single
-- Data.Time.TimeZone object.
getTtInfo :: Get (TtInfo Int)
getTtInfo = do
    gmtoff <- get32bitInt
    isdst <- getBool
    abbrind <- get8bitInt
    return $ TtInfo gmtoff isdst UnknownType abbrind

-- Parse leap second info. (usually not used)
getLeapInfo :: Integral a => Get a -> Get LeapInfo
getLeapInfo getTime = do
    lTime <- fmap toInteger getTime
    lOffset <- get32bitInt
    return $ LeapInfo lTime lOffset

-- Our 8-bit ints are unsigned, so we can convert them directly
get8bitInt :: Get Int
get8bitInt = fmap fromIntegral getWord8

getInt32 :: Get Int32
getInt32 = fmap fromIntegral getWord32be

get32bitInt :: Get Int
get32bitInt = fmap fromIntegral getInt32
                  -- via Int32 to get the sign right
                  -- in case we are on a 64-bit platform

get32bitInteger :: Get Integer
get32bitInteger = fmap fromIntegral getInt32
                  -- via Int32 to get the sign right

getInt64 :: Get Int64
getInt64 = fmap fromIntegral getWord64be

get64bitInteger :: Get Integer
get64bitInteger = fmap fromIntegral getInt64
                  -- via Int64 to get the sign right

getBool :: Get Bool
getBool = fmap (/= 0) getWord8

toASCII :: [Word8] -> String
toASCII = map (toEnum . fromIntegral)

verify :: Monad m => (a -> Bool)  -> String -> a -> m a
verify pred msg val
 | pred val  = return val
 | otherwise = error msg

throwOlson :: FilePath -> String -> IO a
throwOlson fp msg = throw . OlsonError $
                    fp ++ ": invalid timezone file: " ++ msg
