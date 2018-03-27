{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

{-# OPTIONS_GHC
 -Weverything
 -fno-warn-unsafe
 -fno-warn-implicit-prelude
 -fno-warn-missing-import-lists
 -O2
#-}

module Packed.Bytes
  ( Bytes(..)
  , empty
  , singleton
  , cons
  , append
  , pack
  , unpack
  , null
  , drop
  , dropEnd
  , replicate
  , length
  , foldl'
  , take
  , findByte
  , hash
  , hashWith
    -- * Unsliced Byte Arrays
  , toByteArray
  , equalsByteArray
    -- * Characters
  , isAscii
    -- * IO
  , hGetSome
  ) where

import Prelude hiding (take,length,replicate,drop,null)

import Packed.Bytes.Small (ByteArray(..))
import Data.Monoid (Monoid(..))
import Data.Semigroup (Semigroup)
import Data.Word (Word8)
import GHC.Ptr (Ptr(..))
import GHC.Exts (RealWorld,State#,Int#,MutableByteArray#,Addr#,ByteArray#,
  copyAddrToByteArray#,isMutableByteArrayPinned#,(>#))
import GHC.Int (Int(I#))
import GHC.IO (IO(..))
import System.IO (Handle)
import Control.Monad.ST (runST)
import qualified Foreign.Marshal.Alloc as FMA
import qualified Packed.Bytes.Window as BAW
import qualified Packed.Bytes.Small as BA
import qualified System.IO as SIO
import qualified Data.Primitive as PM
import qualified Data.Semigroup as SG

data Bytes = Bytes
  {-# UNPACK #-} !ByteArray -- payload
  {-# UNPACK #-} !Int -- offset
  {-# UNPACK #-} !Int -- length

type Bytes# = (# ByteArray#, Int#, Int# #)

instance Eq Bytes where
  Bytes arrA offA lenA == Bytes arrB offB lenB =
    if lenA == lenB
      then BAW.equality offA offB lenA arrA arrB
      else False

instance Show Bytes where
  show x = "pack " ++ show (unpack x)

instance Semigroup Bytes where
  (<>) = append

instance Monoid Bytes where
  mempty = empty
  mappend = (SG.<>)

cons :: Word8 -> Bytes -> Bytes
cons w (Bytes arr off len) = runST $ do
  marr <- PM.newByteArray (len + 1)
  PM.writeByteArray marr 0 w
  PM.copyByteArray marr 1 arr off len
  newArr <- PM.unsafeFreezeByteArray marr
  return (Bytes newArr 0 (len + 1))

append :: Bytes -> Bytes -> Bytes
append (Bytes arr1 off1 len1) (Bytes arr2 off2 len2) = runST $ do
  marr <- PM.newByteArray (len1 + len2)
  PM.copyByteArray marr 0 arr1 off1 len1
  PM.copyByteArray marr len1 arr2 off2 len2
  arr <- PM.unsafeFreezeByteArray marr
  return (Bytes arr 0 (len1 + len2))

null :: Bytes -> Bool
null (Bytes _ _ len) = len < 1

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

singleton :: Word8 -> Bytes
singleton w = Bytes (BA.singleton w) 0 1

-- empty# :: Bytes#
-- empty# = (# arr, 0#, 0# #)
--   where
--   !(ByteArray arr) = BA.empty

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

hGetSome :: Int -> Handle -> IO Bytes
hGetSome !n !h = do
  !marr <- PM.newByteArray n
  !receivedByteCount <- withBytePtr marr $ \addr -> SIO.hGetBufSome h (addrToPtr addr) n
  !arr <- PM.unsafeFreezeByteArray marr
  let arr' = dwindle# (# unboxByteArray arr, 0#, unboxInt receivedByteCount #)
  return (boxBytes arr')

unboxInt :: Int -> Int#
unboxInt (I# i) = i

boxBytes :: Bytes# -> Bytes
boxBytes (# a, b, c #) = Bytes (ByteArray a) (I# b) (I# c)

withBytePtr ::
     PM.MutableByteArray RealWorld
  -> (PM.Addr -> IO b)
  -> IO b
withBytePtr marr@(PM.MutableByteArray marr#) f =
  case isMutableByteArrayPinned# marr# of
    1# -> do
      let !addr = PM.mutableByteArrayContents marr
      f addr
    _ -> FMA.allocaBytes (PM.sizeofMutableByteArray marr) $ \ptr -> do
      let !addr = ptrToAddr ptr
      !r <- f addr
      copyAddrToByteArray addr marr 0 (PM.sizeofMutableByteArray marr)
      return r

ptrToAddr :: Ptr a -> PM.Addr
ptrToAddr (Ptr x) = PM.Addr x

addrToPtr :: PM.Addr -> Ptr a
addrToPtr (PM.Addr x) = Ptr x

copyAddrToByteArray ::
     PM.Addr
  -> PM.MutableByteArray RealWorld
  -> Int
  -> Int
  -> IO ()
copyAddrToByteArray (PM.Addr addr) (PM.MutableByteArray marr) (I# off) (I# len) =
  IO $ \s0 -> case copyAddrToByteArray# addr marr off len s0 of
    s1 -> (# s1, () #)

dwindle :: Bytes -> Bytes
dwindle b@(Bytes _ _ !len) = if len > 0 then b else empty
  
dwindle# :: Bytes# -> Bytes# 
dwindle# b@(# _,_,len #) = case len ># 0# of
  1# -> b
  _ -> (# unboxByteArray BA.empty, 0#, 0# #)

unboxByteArray :: ByteArray -> ByteArray#
unboxByteArray (ByteArray arr) = arr

