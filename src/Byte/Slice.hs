{-# LANGUAGE BangPatterns #-}

{-# OPTIONS_GHC
 -Weverything
 -fno-warn-unsafe
 -fno-warn-implicit-prelude
 -fno-warn-missing-import-lists
 -O2
#-}

module Byte.Slice
  ( ByteSlice(..)
  , replicate
  , length
  , foldl'
  , take
  , empty
  , findByte
  ) where

import Prelude hiding (take,length,replicate)

import Byte.Array (ByteArray(..))
import Data.Word (Word8)
import qualified Byte.Array.Window as BAW
import qualified Byte.Array as BA

data ByteSlice = ByteSlice
  {-# UNPACK #-} !ByteArray -- payload
  {-# UNPACK #-} !Int -- offset
  {-# UNPACK #-} !Int -- length

replicate :: Int -> Word8 -> ByteSlice
replicate len w = fromByteArray (BA.replicate len w)

fromByteArray :: ByteArray -> ByteSlice
fromByteArray ba = ByteSlice ba 0 (BA.length ba)

length :: ByteSlice -> Int
length (ByteSlice _ _ len) = len

findByte :: Word8 -> ByteSlice -> Maybe Int
findByte !w (ByteSlice arr off len) = BAW.findByte off len w arr

foldl' :: (a -> Word8 -> a) -> a -> ByteSlice -> a
foldl' f !acc0 (ByteSlice arr off len) = BAW.foldl' off len f acc0 arr

take :: Int -> ByteSlice -> ByteSlice
take !n (ByteSlice arr off len) = if n < len
  then ByteSlice arr off (len - n)
  else empty

empty :: ByteSlice
empty = ByteSlice BA.empty 0 0

-- In this implementation, we overallocate on each side to
-- make things line up with machine word boundaries. This
-- make the reversal eight times faster.
-- reverse :: ByteSlice -> ByteSlice
-- reverse (ByteSlice arr off len) =
--   let !leftWordIx = quot off (PM.sizeOf (undefined :: Word))
--       !rightWordIx = quot (off + len) (PM.sizeOf (undefined :: Word))

