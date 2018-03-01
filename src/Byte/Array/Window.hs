{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
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

import Data.Primitive (ByteArray(ByteArray))
import Data.Word (Word8)
import GHC.Types (RuntimeRep,TYPE)
import GHC.Int (Int(I#))
import GHC.Word (Word8(W8#))
import GHC.Exts (Int#,Word#,ByteArray#)
import Data.Bits (xor,(.|.),(.&.),complement,unsafeShiftL)
import qualified Data.Primitive as PM

type Maybe# (a :: TYPE (r :: RuntimeRep)) = (# (# #) | a #)

boxMaybeInt :: Maybe# Int# -> Maybe Int
boxMaybeInt = \case
  (# | a #) -> Just (I# a)
  (# (# #) | #) -> Nothing

unboxInt :: Int -> Int#
unboxInt (I# i) = i

-- | Finds the first occurrence of the given byte.
findByte :: Int -> Int -> Word8 -> ByteArray -> Maybe Int
findByte (I# off) (I# len) (W8# w) (ByteArray arr) =
  boxMaybeInt (findByte' off len w arr)

{-# NOINLINE findByte' #-}
findByte' :: Int# -> Int# -> Word# -> ByteArray# -> Maybe# Int#
findByte' !off# !len0# !w0# !arr0# = 
  let !off = I# off#
      !len0 = I# len0#
      !w0 = W8# w0#
      !arr0 = ByteArray arr0#
      !end0 = off + len0
      !beginMachWord = alignUp off
      !endMachWord = alignDown end0
   in case go off (beginMachWord * PM.sizeOf (undefined :: Word)) w0 arr0 of
        (# | ix #) -> (# | ix #)
        (# (# #) | #) -> case goMachWord beginMachWord endMachWord (broadcastWord8 w0) w0 arr0 of
          (# | ix #) -> (# | ix #)
          (# (# #) | #) -> case go (endMachWord * PM.sizeOf (undefined :: Word)) end0 w0 arr0 of
            (# | ix #) -> (# | ix #)
            (# (# #) | #) -> (# (# #) | #)
  where
  go :: Int -> Int -> Word8 -> ByteArray -> Maybe# Int#
  go !ix !end !w !arr = if ix < end
    then if PM.indexByteArray arr ix == w
      then (# | unboxInt ix #)
      else go (ix + 1) end w arr
    else (# (# #) | #)
  -- The start and end index here are given in Word64 elements,
  -- not Word8 elements.
  goMachWord :: Int -> Int -> Word -> Word8 -> ByteArray -> Maybe# Int#
  goMachWord !ix !end !artifact !w !arr = if ix < end
    then case detectArtifact (unsafeIndexWord arr ix) artifact of
      0 -> goMachWord (ix + 1) end artifact w arr
      _ -> go -- this call to go should always return Just
        (ix * PM.sizeOf (undefined :: Word)) 
        ((ix + 1) * PM.sizeOf (undefined :: Word))
        w arr
    else (# (# #) | #)

-- cast a Word8 index to a machine Word index, rounding up
alignUp :: Int -> Int
alignUp i =
  let !(!quotient,!remainder) = quotRem i (PM.sizeOf (undefined :: Word))
   in case remainder of
        0 -> quotient
        _ -> quotient + 1

-- cast a Word8 index to a machine Word index, rounding down
alignDown :: Int -> Int
alignDown i = quot i (PM.sizeOf (undefined :: Word))

broadcastWord8 :: Word8 -> Word
broadcastWord8 !w0 = go 8 (fromIntegral w0) where
  go :: Int -> Word -> Word
  go !n !w = if n < 8 * PM.sizeOf (undefined :: Word)
    then go (twice n) (unsafeShiftL w n .|. w)
    else w

twice :: Int -> Int
twice n = n * 2

-- returns non-zero if a null byte is present in the machine word
detectNull :: Word -> Word
detectNull x = (x - repeatHexZeroOne) .&. complement x .&. repeatHexEightZero

detectArtifact :: Word -> Word -> Word
detectArtifact x artifact = detectNull (applyArtifact x artifact)

applyArtifact :: Word -> Word -> Word
applyArtifact = xor

repeatHexZeroOne :: Word
repeatHexZeroOne = div maxBound 255

repeatHexEightZero :: Word
repeatHexEightZero = 128 * (div maxBound 255 :: Word)

foldl' :: forall a. Int -> Int -> (a -> Word8 -> a) -> a -> ByteArray -> a
foldl' !off !len f !acc0 !arr = go acc0 off where
  go :: a -> Int -> a
  go !acc !ix = if ix < off + len
    then go (f acc (PM.indexByteArray arr ix)) (ix + 1)
    else acc

-- this is only used internally
unsafeIndexWord :: ByteArray -> Int -> Word
unsafeIndexWord = PM.indexByteArray

-- TODO: optimize this. We could do a whole Word64 at a
-- time if the bytearray is pinned. Maybe even if it
-- isn't pinned.
-- reverse :: Int -> Int -> ByteArray -> ByteArray
-- reverse off len arr = runST

