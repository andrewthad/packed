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
  ) where

import Prelude hiding (reverse)

import Packed.Bytes.Small (ByteArray)
import Data.Bits ((.&.))
import Data.Semigroup (Semigroup)
import Data.Primitive (PrimUnlifted)
import qualified Packed.Bytes.Small as BA
import qualified Data.Semigroup as SG
import qualified Data.Primitive as PM
import qualified Packed.Bytes.Window as BAW

newtype SmallText = SmallText ByteArray 
  deriving (Eq,Semigroup,Monoid,PrimUnlifted)

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


