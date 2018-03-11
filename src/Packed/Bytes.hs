{-# LANGUAGE BangPatterns #-}

{-# OPTIONS_GHC
 -Weverything
 -fno-warn-unsafe
 -fno-warn-implicit-prelude
 -fno-warn-missing-import-lists
 -O2
#-}

module Packed.Bytes
  ( Bytes(..)
  , pack
  , unpack
  , drop
  , dropEnd
  , replicate
  , length
  , foldl'
  , take
  , empty
  , findByte
  , hash
  , hashWith
    -- * Unsliced Byte Arrays
  , toByteArray
  , equalsByteArray
    -- * Characters
  , isAscii
  ) where

import Prelude hiding (take,length,replicate,drop)

import Packed.Bytes.Small (ByteArray(..))
import Data.Word (Word8)
import qualified Packed.Bytes.Window as BAW
import qualified Packed.Bytes.Small as BA

data Bytes = Bytes
  {-# UNPACK #-} !ByteArray -- payload
  {-# UNPACK #-} !Int -- offset
  {-# UNPACK #-} !Int -- length

instance Eq Bytes where
  Bytes arrA offA lenA == Bytes arrB offB lenB =
    if lenA == lenB
      then BAW.equality offA offB lenA arrA arrB
      else False

instance Show Bytes where
  show x = "pack " ++ show (unpack x)

pack :: [Word8] -> Bytes
pack bs = let arr = BA.pack bs in Bytes arr 0 (BA.length arr)

unpack :: Bytes -> [Word8]
unpack (Bytes arr off len) = go off
  where
  go :: Int -> [Word8]
  go !ix = if ix < len + off
    then BA.unsafeIndex arr ix : go (ix + 1)
    else []

drop :: Int -> Bytes -> Bytes
drop n (Bytes arr off len) = if len > n
  then Bytes arr (off + n) (len - n)
  else empty

dropEnd :: Int -> Bytes -> Bytes
dropEnd n (Bytes arr off len) = if len > n
  then Bytes arr off (len - n)
  else empty

replicate :: Int -> Word8 -> Bytes
replicate len w = fromByteArray (BA.replicate len w)

fromByteArray :: ByteArray -> Bytes
fromByteArray ba = Bytes ba 0 (BA.length ba)

length :: Bytes -> Int
length (Bytes _ _ len) = len

findByte :: Word8 -> Bytes -> Maybe Int
findByte !w (Bytes arr off len) = case BAW.findByte off len w arr of
  Just ix -> Just (ix - off)
  Nothing -> Nothing

foldl' :: (a -> Word8 -> a) -> a -> Bytes -> a
foldl' f !acc0 (Bytes arr off len) = BAW.foldl' off len f acc0 arr

take :: Int -> Bytes -> Bytes
take !n (Bytes arr off len) = if n < len
  then Bytes arr off (len - n)
  else empty

empty :: Bytes
empty = Bytes BA.empty 0 0

isAscii :: Bytes -> Bool
isAscii (Bytes arr off len) = BAW.isAscii off len arr

hash ::
     Int -- ^ buckets
  -> Bytes -- ^ array
  -> Int
hash = hashWith 0

hashWith ::
     Int -- ^ salt
  -> Int -- ^ buckets
  -> Bytes -- ^ array
  -> Int
hashWith salt buckets (Bytes arr off len) =
  BAW.hashWith off len salt buckets arr

-- In this implementation, we overallocate on each side to
-- make things line up with machine word boundaries. This
-- make the reversal eight times faster.
-- reverse :: Bytes -> Bytes
-- reverse (Bytes arr off len) =
--   let !leftWordIx = quot off (PM.sizeOf (undefined :: Word))
--       !rightWordIx = quot (off + len) (PM.sizeOf (undefined :: Word))

-- | Copy the 'Bytes', discarded unneeded data outside of
--   the slice.
toByteArray :: Bytes -> ByteArray
toByteArray (Bytes arr off len) = BAW.slice off len arr

-- | Check for equality of sliced 'Bytes' and an unsliced 'ByteArray'.
equalsByteArray :: ByteArray -> Bytes -> Bool
equalsByteArray arr1 (Bytes arr2 off2 len2) =
  if BA.length arr1 == len2
    then BAW.equals 0 off2 len2 arr1 arr2
    else False
