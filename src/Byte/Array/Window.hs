{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}

{-# OPTIONS_GHC
 -Weverything
 -fno-warn-unsafe
 -fno-warn-implicit-prelude
 -O2
#-}

module Byte.Array.Window
  ( findByte
  , foldl'
  , reverse
  ) where

import Data.Primitive (ByteArray)
import Data.Word (Word8)
import GHC.Types (RuntimeRep,TYPE)
import GHC.Int (Int(I#))
import GHC.Exts (Int#)
import qualified Data.Primitive as PM

type Maybe# (a :: TYPE (r :: RuntimeRep)) = (# (# #) | a #)

boxMaybeInt :: Maybe# Int# -> Maybe Int
boxMaybeInt = \case
  (# | a #) -> Just (I# a)
  (# (# #) | #) -> Nothing

unboxInt :: Int -> Int#
unboxInt (I# i) = i

-- | Finds the first occurrence of the given byte.
--   TODO: optimize this to search through a whole
--   Word64 at a time.
findByte :: Int -> Int -> Word8 -> ByteArray -> Maybe Int
findByte !off !len0 !w0 !arr0 = boxMaybeInt (go off (len0 + off) w0 arr0) where
  go :: Int -> Int -> Word8 -> ByteArray -> Maybe# Int#
  go !ix !end !w !arr = if ix < end
    then if PM.indexByteArray arr ix == w
      then (# | unboxInt ix #)
      else go (ix + 1) end w arr
    else (# (# #) | #)
  

foldl' :: forall a. Int -> Int -> (a -> Word8 -> a) -> a -> ByteArray -> a
foldl' !off !len f !acc0 !arr = go acc0 off where
  go :: a -> Int -> a
  go !acc !ix = if ix < off + len
    then go (f acc (PM.indexByteArray arr ix)) (ix + 1)
    else acc

-- TODO: optimize this. We could do a whole Word64 at a
-- time if the bytearray is pinned. Maybe even if it
-- isn't pinned.
-- reverse :: Int -> Int -> ByteArray -> ByteArray
-- reverse off len arr = runST

