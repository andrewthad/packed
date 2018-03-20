{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC
 -Weverything
 -fno-warn-unsafe
 -fno-warn-implicit-prelude
 -fno-warn-missing-import-lists
 -O2
#-}
module Packed.Text.Small
  ( SmallText(..)
  , empty
  , reverse
  , decodeAscii
  ) where

import Prelude hiding (reverse)

import Packed.Bytes.Small (ByteArray)
import Data.Bits ((.&.))
import Data.Semigroup (Semigroup)
import qualified Packed.Bytes.Small as BA
import qualified Data.Semigroup as SG
import qualified Data.Primitive as PM
import qualified Packed.Bytes.Window as BAW

newtype SmallText = SmallText ByteArray 

-- Text is UTF-8 encoded with one caveat. There are several operations
-- that can be faster if we know in advance that all the codepoints in
-- a piece of text are in the ascii range. So, we use the first
-- byte to encode the additional information. If the first byte has
-- zero as the MSB (representing a code point in the ascii range),
-- it is assumed that the entire piece of text only uses code points
-- in the ascii range. If however, the first byte is 0b11111111
-- (which is not valid a UTF-8 byte anywhere), then this first byte is discarded
-- and it is assumed that the text contains code points from any range.
-- The first byte is not allowed to be anything starting with 1.
-- This bloats all non-English text by one byte.

instance Eq SmallText where
  -- Change this to just use the Eq instance for ByteArray
  -- once primitive-0.6.3 becomes common.
  SmallText a == SmallText b = if PM.sizeofByteArray a == PM.sizeofByteArray b
    then BAW.equals 0 0 (PM.sizeofByteArray a) a b
    else False

-- This is a boolean but with a more efficient way to
-- do conjunction
newtype UnicodeRange = UnicodeRange Word

ascii :: UnicodeRange
ascii = UnicodeRange 1

nonAscii :: UnicodeRange
nonAscii = UnicodeRange 0

instance Semigroup UnicodeRange where
  UnicodeRange a <> UnicodeRange b = UnicodeRange (a .&. b)

instance Monoid UnicodeRange where
  mempty = ascii
  mappend = (SG.<>)

unicodeRange :: ByteArray -> UnicodeRange
unicodeRange arr
  | BA.length arr > 0 =
      case BA.unsafeIndex arr 0 of
        0b11111111 -> nonAscii
        _ -> ascii
  | otherwise = ascii

empty :: SmallText
empty = SmallText BA.empty

reverse :: SmallText -> SmallText
reverse (SmallText arr) = case unicodeRange arr of
  UnicodeRange 1 -> SmallText (BA.reverse arr)
  _ -> error "Text.Array.reverse: write the non-ascii case"

decodeAscii :: ByteArray -> Maybe SmallText
decodeAscii arr = if BA.isAscii arr
  then Just (SmallText arr)
  else Nothing


