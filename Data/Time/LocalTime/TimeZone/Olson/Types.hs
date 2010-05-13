-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Time.LocalTime.TimeZone.Olson.Types
-- Copyright   :  Yitzchak Gale 2010
--
-- Maintainer  :  Yitzchak Gale <gale@sefer.org>
-- Portability :  portable
--
-- Data types to represent timezone data as used in Olson timezone
-- files, and as specified by the tzfile(5) man page on Unix-like
-- systems. For more information about this format, see
-- http://www.twinsun.com/tz/tz-link.htm.
--
-- Both Version 1 and Version 2 timezone data can be represented.

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
-}

module Data.Time.LocalTime.TimeZone.Olson.Types
(
 OlsonData(..),
 Transition(..),
 TransitionType(..),
 TtInfo(..),
 LeapInfo(..)
)
where

import Data.Monoid (Monoid(..))
import Control.Monad (mplus)

-- | @OlsonData@ represents a full set of timezone data for a location.
--
-- @OlsonData@ can represent timezone data from files in Version 1 format
-- or Version 2 format. Version 1 format files can only contain timestamp
-- values that can be represented in less than 32 bits, and cannot contain
-- a POSIX TZ string.
--
-- In a Version 2 format file, the timezone data is split into two parts.
-- The first part contains timezone data for which all timestamp values
-- can be represented in less 32 bits, and the second part contains
-- timezone data for which 32 bits or more are required to represent
-- timestamp values. The POSIX TZ string, if present, can only be rendered
-- in a Version 2 file, and appears after both parts of the timezone data.
data OlsonData =
    OlsonData
      [Transition]
      [TtInfo String]
      [LeapInfo]
      (Maybe String) -- ^ Optional POSIX TZ string for the last @Transition@
  deriving (Eq, Show)

instance Monoid OlsonData where
  mempty = OlsonData [] [] [] Nothing
  mappend (OlsonData a  b  c  d ) (OlsonData a' b' c' d')
    = OlsonData (a++a') (b++b') (c++c') (d `mplus` d')

-- | A @Transition@ represents a moment when the clocks change in a
-- timezone. It consists of a Unix timestamp value that indicates the
-- exact moment in UTC when the clocks change, and the 0-based index
-- in the list of @TtInfo@ specifications for the description of the
-- new time after the clocks change.
data Transition =
       Transition
         {transTime :: Integer, -- ^ Unix timestamp indicating the time
                                -- when the clocks change
          transIndex :: Int}    -- ^ 0-based index in the list of @TtInfo@
                                -- that describes the new time
  deriving (Eq, Show)

-- | A @TransitionType@ is historical information about whether the
-- official body that announced a time change specified the time of
-- the change in terms of UTC, standard time (i.e., non-summer time)
-- for the time zone, or the current wall clock time in the time
-- zone. This historical trivia may seem rather boring, but
-- unfortunately it is needed to interpret a POSIX-style TZ string
-- timezone specification correctly.
data TransitionType = Std | Wall | UTC | UnknownType
  deriving (Eq, Ord, Show)

-- | A @TtInfo@ is a specification of local time in a timezone for
-- some period during which the clocks did not change. `abbr` is
-- @String@ if the timezone abbreviation is represented as a @String@,
-- or @Int@ if it is represented as an index into a long string of
-- null-terminated abbreviation strings (as in an Olson binary
-- timezone file).
data TtInfo abbr = 
       TtInfo
         {tt_gmtoff :: Int, -- ^ The offset of local clocks from UTC,
                            -- in seconds
          tt_isdst :: Bool, -- ^ True if local clocks are summer time
          tt_ttype :: TransitionType,
          tt_abbr :: abbr} -- ^ The timezone abbreviation string.
  deriving (Eq, Show)

-- | Olson timezone files can contain leap second specifications, though
-- the official files have not done so since the late 1980s.
data LeapInfo =
       LeapInfo
         {leapTime :: Integer, -- ^ A Unix timestamp indicating the time
                               -- that the leap second occurred
          leapOffset :: Int}   -- ^ The new total offset of UTC from UT1
                               -- after this leap second
  deriving (Eq, Show)
