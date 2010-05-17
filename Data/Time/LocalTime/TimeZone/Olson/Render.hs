{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Time.LocalTime.TimeZone.Olson.Render
-- Copyright   :  Yitzchak Gale 2010
--
-- Maintainer  :  Yitzchak Gale <gale@sefer.org>
-- Portability :  portable
--
-- Render Olson timezone data in the standard binary format as
-- used in Olson timezone files, and as specified by the
-- tzfile(5) man page on Unix-like systems. For more information
-- about this format, see http://www.twinsun.com/tz/tz-link.htm.

{- Copyright (c) 2010 Yitzchak Gale. All rights reserved.
For licensing information, see the BSD3-style license in the file
LICENSE that was originally distributed by the author together with
this file. -}

module Data.Time.LocalTime.TimeZone.Olson.Render
(
 -- * Rendering Olson timezone files
 -- | If any of the transition times or leap second times specified
 -- require more than a 32-bit integer to represent as a Unix
 -- timestamp, or if a POSIX-style TZ string is specified, timezone
 -- data is rendered using Version 2 format. Otherwise, the timezone data
 -- is rendered using Version 1 format.
 renderOlsonToFile,
 putOlson,
 splitOlson
)
where

import Data.Time.LocalTime.TimeZone.Olson.Types
import Data.Time.LocalTime.TimeZone.Series (TimeZoneSeries(TimeZoneSeries))
import Data.Time (TimeZone(TimeZone, timeZoneSummerOnly, timeZoneName))
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Binary.Put (Put, runPut, putByteString, putWord8, flush,
                        putWord32be, putWord64be)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.List (partition, sortBy, sort, group)
import Data.Ord (comparing)
import Data.Word (Word8)
import Data.Maybe (listToMaybe, maybeToList, isNothing, fromMaybe, catMaybes)
import Data.Monoid (mempty)
import Control.Monad (guard, replicateM_)

-- | Render a @TimeZoneSeries@ as a binary Olson timezone file.
renderTimeZoneSeriesToOlsonFile :: FilePath -> TimeZoneSeries -> IO ()
renderTimeZoneSeriesToOlsonFile fp = renderOlsonToFile fp .
  fromMaybe (error "Cannot render TimeZoneSeries: default is summer time") .
  timeZoneSeriesToOlson

-- | Convert a @TimeZoneSeries@ to @OlsonData@ for rendering.
timeZoneSeriesToOlson :: TimeZoneSeries -> Maybe OlsonData
timeZoneSeriesToOlson (TimeZoneSeries dflt pairs)
 | timeZoneSummerOnly dflt && not (all timeZoneSummerOnly $ map snd pairs)
             = Nothing
 | otherwise = Just $
    OlsonData
      [Transition secs ttinfo |
         (t, tzs) <- pairs,
         let secs = round $ utcTimeToPOSIXSeconds t,
         ttinfo <- maybeToList $ lookup (mkTT tzs) ttAssocs]
      ttinfos
      []
      Nothing
  where
    mkTT (TimeZone offset issdst abbr) =
      TtInfo (offset*60) issdst Wall abbr
    dfltTT = mkTT dflt
    ttAssocs = (dfltTT, 0) :
               zip (filter (/= dfltTT) $ map (mkTT . snd) pairs) [1..]
    ttinfos = map fst ttAssocs

-- | Render Olson timezone data as a binary Olson timezone file
renderOlsonToFile :: FilePath -> OlsonData -> IO ()
renderOlsonToFile fp = L.writeFile fp . runPut . putOlson

-- | Render Olson timezone data in binary Olson timezone file format
-- as a lazy @ByteString@
putOlson :: OlsonData -> Put
putOlson olson = putOlsonParts version olson1 olson2 posix >> flush
  where
    (olson1, olson2, posix) = splitOlson olson
    version
     | olson2 /= mempty = 50
     | otherwise        = maybe 0 (const 50) $ posix >>= guard . not . null

-- | Split Olson timezone data into three parts: timezone data that can
-- be rendered using Version 1 format, timezone data that can only be
-- rendered using Version 2 format, and the POSIX-style TZ string
-- if specified
splitOlson :: OlsonData -> (OlsonData, OlsonData, Maybe String)
splitOlson (OlsonData transs ttinfos leaps posix) =
    (OlsonData transs1 ttinfos1 leaps1 Nothing,
     OlsonData transs2 ttinfos2 leaps2 Nothing,
     posix)
  where
    cutoff = 0x80000000 -- 2^31
    fitsIn32bits x = x < cutoff && x >= negate cutoff
    ( leaps1 ,  leaps2 ) = partition (fitsIn32bits .  leapTime)  leaps
    (transs1', transs2') = partition (fitsIn32bits . transTime) transs
    assoc1 = mkAssoc [0] transs1'
    assoc2 = mkAssoc []  transs2'
    transs1 = mkTranss transs1' assoc1
    transs2 = mkTranss transs2' assoc2
    ttinfos1 = mkTtinfos assoc1
    ttinfos2 = mkTtinfos assoc2
    mkAssoc prepend transs' = zip
      (sortBy (comparing $ fmap tt_ttype . listToMaybe . flip drop ttinfos) .
        uniq . sort . (prepend ++) $ map transIndex transs')
      [0..]
    mkTranss transs' assoc = [t {transIndex = i} |
      t <- transs', i <- maybeToList $ lookup (transIndex t) assoc]
    mkTtinfos assoc = map snd . dropWhile (isNothing . fst) .
      sortBy (comparing fst) $ zip (map (flip lookup assoc) [0..]) ttinfos

putOlsonParts :: Word8 -> OlsonData -> OlsonData -> Maybe String -> Put
putOlsonParts 0  olson1 _      _     = putOlsonPart 0 put32bitIntegral olson1
putOlsonParts v2 olson1 olson2 posix = do
  putOlsonPart v2 put32bitIntegral olson1
  putOlsonPart v2 put64bitIntegral olson2
  putPosixTZ posix

putOlsonPart :: Word8 -> (Integer -> Put) -> OlsonData -> Put
putOlsonPart version putTime (OlsonData transs ttinfos leaps _) = do
    putASCII "TZif"
    putWord8 version
    putByteString . B.pack $ replicate 15 0 -- padding nulls
    replicateM_ 2 $ putCount ttinfosWithTtype
                       -- tzh_ttisgmtcnt
                       -- tzh_ttisstdcnt
    putCount leaps     -- tzh_leapcnt
    putCount transs    -- tzh_timecnt
    putCount ttinfos   -- tzh_typecnt
    putCount abbrChars -- tzh_charcnt
    mapM_ (putTime         . transTime ) transs
    mapM_ (put8bitIntegral . transIndex) transs
    mapM_ putTtInfo ttinfosIndexed
    putASCII abbrChars
    mapM_ (putLeapInfo putTime) leaps
    mapM_ (putBool . (== Std) . tt_ttype) ttinfosWithTtype -- isstd
    mapM_ (putBool . (== UTC) . tt_ttype) ttinfosWithTtype -- isgmt
  where
    putCount = put32bitIntegral . length
    ttinfosWithTtype = takeWhile ((<= UTC) . tt_ttype) ttinfosIndexed
    abbrStrings = uniq . sort $ map tt_abbr ttinfos
    abbrChars = concatMap (++ "\NUL") abbrStrings
    ttinfosIndexed = [TtInfo gmtoff isdst ttype i |
      TtInfo gmtoff isdst ttype abbr <- ttinfos,
      i <- maybeToList . lookup abbr . zip abbrStrings .
           scanl (+) 0 $ map ((+ 1) . length) abbrStrings]

putPosixTZ :: Maybe String -> Put
putPosixTZ posix = do
  putWord8 10
  putASCII $ fromMaybe "" posix
  putWord8 10

putTtInfo :: TtInfo Int -> Put
putTtInfo tt = do
    put32bitIntegral $ tt_gmtoff tt
    putBool $ tt_isdst tt
    put8bitIntegral $ tt_abbr tt

putLeapInfo :: Integral a => (a -> Put) -> LeapInfo -> Put
putLeapInfo putTime leap = do
    putTime . fromIntegral $ leapTime leap
    put32bitIntegral $ leapOffset leap

-- Converting signed integrals to unsigned can be done directly,
-- without the care needed for the opposite direction when parsing.

put8bitIntegral :: Integral a => a -> Put
put8bitIntegral = putWord8 . fromIntegral

put32bitIntegral :: Integral a => a -> Put
put32bitIntegral = putWord32be . fromIntegral

put64bitIntegral :: Integral a => a -> Put
put64bitIntegral = putWord64be . fromIntegral

putBool :: Bool -> Put
putBool False = putWord8 0
putBool True  = putWord8 1

uniq :: Eq a => [a] -> [a]
uniq = map head . group

putASCII :: String -> Put
putASCII = putByteString . B.pack . map (fromIntegral . fromEnum)
