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
  , zipAnd
  , zipOr
  , zipXor
  ) where

import Data.Primitive (ByteArray(ByteArray))
import Data.Word (Word8)
import GHC.Types (RuntimeRep,TYPE)
import GHC.Int (Int(I#))
import GHC.Word (Word8(W8#))
import GHC.Exts (Int#,Word#,ByteArray#)
import Data.Bits (xor,(.|.),(.&.),complement,unsafeShiftL)
import Control.Monad.ST (ST,runST)
import qualified Data.Primitive as PM

type Maybe# (a :: TYPE (r :: RuntimeRep)) = (# (# #) | a #)

boxMaybeInt :: Maybe# Int# -> Maybe Int
boxMaybeInt = \case
  (# | a #) -> Just (I# a)
  (# (# #) | #) -> Nothing

unboxInt :: Int -> Int#
unboxInt (I# i) = i

-- | Finds the first occurrence of the given byte.
{-# INLINE findByte #-}
findByte :: Int -> Int -> Word8 -> ByteArray -> Maybe Int
findByte (I# off) (I# len) (W8# w) (ByteArray arr) =
  boxMaybeInt (findByte' off len w arr)

{-# NOINLINE findByte' #-}
findByte' :: Int# -> Int# -> Word# -> ByteArray# -> Maybe# Int#
findByte' !off# !len0# !w0# !arr0# = 
  let !off = I# off#
      !len0 = I# len0#
      !end0 = off + len0
      !beginMachWord = alignUp off
      !endMachWord = alignDown end0
   in case go off (beginMachWord * PM.sizeOf (undefined :: Word)) of
        (# | ix #) -> (# | ix #)
        (# (# #) | #) -> case goMachWord beginMachWord endMachWord (broadcastWord8 w) of
          (# | ix #) -> (# | ix #)
          (# (# #) | #) -> case go (endMachWord * PM.sizeOf (undefined :: Word)) end0 of
            (# | ix #) -> (# | ix #)
            (# (# #) | #) -> (# (# #) | #)
  where
  !w = W8# w0#
  !arr = ByteArray arr0#
  go :: Int -> Int -> Maybe# Int#
  go !ix !end = if ix < end
    then if PM.indexByteArray arr ix == w
      then (# | unboxInt ix #)
      else go (ix + 1) end
    else (# (# #) | #)
  -- The start and end index here are given in Word64 elements,
  -- not Word8 elements.
  goMachWord :: Int -> Int -> Word -> Maybe# Int#
  goMachWord !ix !end !artifact = if ix < end
    then case detectArtifact (unsafeIndexWord arr ix) artifact of
      0 -> goMachWord (ix + 1) end artifact
      _ -> go -- this call to go should always return Just
        (ix * PM.sizeOf (undefined :: Word)) 
        ((ix + 1) * PM.sizeOf (undefined :: Word))
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

-- this is only used internally
unsafeIndex :: ByteArray -> Int -> Word8
unsafeIndex = PM.indexByteArray

-- TODO: optimize this. We could do a whole Word64 at a
-- time if the bytearray is pinned. Maybe even if it
-- isn't pinned.
-- reverse :: Int -> Int -> ByteArray -> ByteArray
-- reverse off len arr = runST

{-# INLINE zipVectorizable #-}
zipVectorizable ::
     (Word8 -> Word8 -> Word8)
  -> (Word -> Word -> Word)
  -> Int -- start x
  -> Int -- len x
  -> Int -- start y
  -> Int -- len y
  -> ByteArray -- x
  -> ByteArray -- y
  -> ByteArray -- z
zipVectorizable !combine !combineMach !startX !lenX !startY !lenY !x !y = runST action
  where
  action :: forall s. ST s ByteArray
  action = do
    let !len = min lenX lenY
    marr <- PM.newByteArray len
    let !(!quotStartX,!remStartX) = quotRem startX (PM.sizeOf (undefined :: Word))
        !(!quotStartY,!remStartY) = quotRem startY (PM.sizeOf (undefined :: Word))
        go :: Int -> Int -> ST s ()
        go !ix !end = if ix < end
          then do
            PM.writeByteArray marr ix (combine (unsafeIndex x (startX + ix)) (unsafeIndex y (startY + ix)))
            go (ix + 1) end
          else return ()
        goMach :: Int -> Int -> ST s ()
        goMach !ix !end = if ix < end
          then do
            PM.writeByteArray marr ix (combineMach (unsafeIndexWord x (quotStartX + ix)) (unsafeIndexWord y (quotStartY + ix)))
            goMach (ix + 1) end
          else return ()
    if remStartX .|. remStartY == 0 -- if they are both zero
      then do
        let !lenQuotient = quot len (PM.sizeOf (undefined :: Word))
        goMach 0 lenQuotient
        go (lenQuotient * PM.sizeOf (undefined :: Word)) len
      else go 0 len
    PM.unsafeFreezeByteArray marr

zipAnd :: Int -> Int -> Int -> Int -> ByteArray -> ByteArray -> ByteArray
zipAnd x0 xlen y0 ylen x y = zipVectorizable (.&.) (.&.) x0 xlen y0 ylen x y

zipOr :: Int -> Int -> Int -> Int -> ByteArray -> ByteArray -> ByteArray
zipOr x0 xlen y0 ylen x y = zipVectorizable (.|.) (.|.) x0 xlen y0 ylen x y

zipXor :: Int -> Int -> Int -> Int -> ByteArray -> ByteArray -> ByteArray
zipXor x0 xlen y0 ylen x y = zipVectorizable (.|.) (.|.) x0 xlen y0 ylen x y

