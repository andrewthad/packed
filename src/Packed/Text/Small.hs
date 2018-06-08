{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
  , decodeAscii
  , encodeUtf8
  , pack
  , unpack
  ) where

import Prelude hiding (reverse)

import Packed.Bytes.Small (ByteArray)
import Data.Semigroup (Semigroup)
import Data.Primitive (PrimUnlifted)
import Data.String (IsString(fromString))
import qualified Packed.Bytes.Small as BA
import qualified Packed.Text.Window as TW

newtype SmallText = SmallText ByteArray 
  deriving (Eq,Ord,Semigroup,Monoid,PrimUnlifted)

instance Show SmallText where
  showsPrec p ps r = showsPrec p (unpack ps) r

instance IsString SmallText where
  fromString = pack

-- Text is UTF-8 encoded. Unlike the type used for sliced text,
-- this type does not have any way to track whether or not any non-ascii
-- characters are present in the text. The functions take, drop, reverse,
-- and index do not get the same speed ups they do for the sliced variant.

empty :: SmallText
empty = SmallText BA.empty

decodeAscii :: ByteArray -> Maybe SmallText
decodeAscii arr = if BA.isAscii arr
  then Just (SmallText arr)
  else Nothing

encodeUtf8 :: SmallText -> ByteArray
encodeUtf8 (SmallText t) = t

unpack :: SmallText -> String
unpack (SmallText t) = TW.unpack 0 (BA.length t) t

pack :: String -> SmallText
pack s = let (arr,_) = TW.pack s in SmallText arr

