{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC
 -Weverything
 -fno-warn-unsafe
 -fno-warn-implicit-prelude
 -fno-warn-missing-import-lists
 -O2
#-}

module Packed.Bytes.Small
  ( ByteArray(..)
  , empty
  , pack
  , unpack
  , singleton
  , append
  , concatReversed
  , replicate
  , length
  , findByte
  , foldl'
  , take
  , cons
  , uncons
  , reverse
  , hash
  , hashWith
    -- * Binary Encodings
  , bigEndianWord16
  , bigEndianWord32
  , littleEndianWord32
    -- * Zip
  , zipAnd
  , zipOr
  , zipXor
    -- * Characters
  , isAscii
    -- * IO
  , hGet
  , hGetSome
  , readFile
    -- * Unsafe
  , unsafeIndex
  ) where

import Prelude hiding (replicate,length,take,reverse,readFile)

import Control.Exception (IOException,catch)
import Control.Monad.Primitive (PrimMonad,PrimState,primitive_)
import Control.Monad.ST (runST,ST)
import Data.Bits (FiniteBits,unsafeShiftR,finiteBitSize)
import Data.Primitive.ByteArray (ByteArray(..),MutableByteArray(..))
import GHC.Exts (RealWorld,ByteArray#,setByteArray#,word2Int#,byteSwap#)
import GHC.Exts (shrinkMutableByteArray#,isMutableByteArrayPinned#)
import GHC.Int (Int(I#))
import GHC.Ptr (Ptr(..))
import GHC.Word (Word8(W8#),Word(W#),Word32,Word16)
import GHC.IO.Handle (hFileSize)
import System.IO (Handle)
import qualified Foreign.Marshal.Alloc as FMA
import qualified GHC.OldList as L
import qualified Data.List.Unlifted as LU
import qualified Packed.Bytes.Window as BAW
import qualified Data.Primitive as PM
import qualified System.IO as SIO

singleton :: Word8 -> ByteArray
singleton w = runST $ do
  marr <- PM.newByteArray 1
  PM.writeByteArray marr 0 w
  PM.unsafeFreezeByteArray marr

pack :: [Word8] -> ByteArray
pack ws0 = runST $ do
  marr <- PM.newByteArray (L.length ws0)
  let go [] !_ = return ()
      go (w : ws) !ix = writeByteArrayWord8 marr ix w >> go ws (ix + 1)
  go ws0 0
  PM.unsafeFreezeByteArray marr

unpack :: ByteArray -> [Word8]
unpack arr = go 0 where
  go :: Int -> [Word8]
  go !ix = if ix < length arr
    then unsafeIndex arr ix : go (ix + 1)
    else []

append :: ByteArray -> ByteArray -> ByteArray
append !a !b = do
  let !lenA = length a
      !lenB = length b
      !len = lenA + lenB
   in runST $ do
        !marr <- PM.newByteArray len
        PM.copyByteArray marr 0 a 0 lenA
        PM.copyByteArray marr lenA b 0 lenB
        PM.unsafeFreezeByteArray marr

cons :: Word8 -> ByteArray -> ByteArray
cons w arr = runST $ do
  let !lenArr = length arr
  marr <- PM.newByteArray (lenArr + 1)
  PM.writeByteArray marr 0 w
  PM.copyByteArray marr 1 arr 0 lenArr
  PM.unsafeFreezeByteArray marr

uncons :: ByteArray -> Maybe (Word8, ByteArray)
uncons arr0 = if lenArr > 0
  then
    let !arr1 = runST $ do
          marr <- PM.newByteArray (lenArr - 1)
          PM.copyByteArray marr 0 arr0 1 (lenArr - 1)
          PM.unsafeFreezeByteArray marr
        w :: Word8
        !w = PM.indexByteArray arr0 0
     in Just (w,arr1)
  else Nothing
  where
  !lenArr = length arr0

concatReversed :: [ByteArray] -> ByteArray
concatReversed arrs = runST $ do
  let len = sumLengths arrs 0
  marr <- PM.newByteArray len
  pasteReversedByteArrays marr len arrs
  PM.unsafeFreezeByteArray marr

-- internal function
pasteReversedByteArrays :: MutableByteArray s -> Int -> [ByteArray] -> ST s ()
pasteReversedByteArrays !_ !_ [] = return ()
pasteReversedByteArrays !marr !ix (x : xs) = do
  let nextIx = ix - length x
  PM.copyByteArray marr nextIx x 0 (length x)
  pasteReversedByteArrays marr nextIx xs

-- internal function
sumLengths :: [ByteArray] -> Int -> Int
sumLengths = go
  where
  go [] !n = n
  go (x : xs) !n = sumLengths xs (length x + n)

replicate :: Int -> Word8 -> ByteArray
replicate len@(I# len#) (W8# w#) = runST $ do
  marr@(MutableByteArray marr#) <- PM.newByteArray len
  primitive_ (setByteArray# marr# 0# len# (word2Int# w#))
  PM.unsafeFreezeByteArray marr

length :: ByteArray -> Int
length = PM.sizeofByteArray

findByte :: Word8 -> ByteArray -> Maybe Int
findByte !w !arr = BAW.findByte 0 (length arr) w arr

foldl' :: (a -> Word8 -> a) -> a -> ByteArray -> a
foldl' f !acc0 !arr = BAW.foldl' 0 (length arr) f acc0 arr

take :: Int -> ByteArray -> ByteArray
take !n !arr = if n < length arr
  then runST $ do
    marr <- PM.newByteArray n
    PM.copyByteArray marr 0 arr 0 n
    PM.unsafeFreezeByteArray marr
  else empty

empty :: ByteArray
empty = runST (PM.newByteArray 0 >>= PM.unsafeFreezeByteArray)

-- | Reverse the bytes in a byte array. This operation is eight
--   times faster if the length of the byte array divides eight
--   evenly.
reverse :: ByteArray -> ByteArray
reverse !arr = runST $ do
  marr <- PM.newByteArray (length arr)
  let !(!quotient,!remainder) = quotRem (length arr) (PM.sizeOf (undefined :: Word))
  if remainder == 0
    then goFast 0 quotient marr
    else goSlow 0 (length arr) marr
  PM.unsafeFreezeByteArray marr
  where
  goFast :: forall s. Int -> Int -> MutableByteArray s -> ST s ()
  goFast !ix !len !marr = if ix < len
    then do
      let !w = unsafeIndexWord arr ix
      writeByteArrayWord marr (len - ix) (byteSwap w)
      goFast (ix + 1) len marr
    else return ()
  goSlow :: forall s. Int -> Int -> MutableByteArray s -> ST s ()
  goSlow !ix !len !marr = if ix < len
    then do
      let !w = unsafeIndex arr ix
      writeByteArrayWord8 marr (len - ix) w
      goSlow (ix + 1) len marr
    else return ()

byteSwap :: Word -> Word
byteSwap (W# w) = W# (byteSwap# w)

writeByteArrayWord :: MutableByteArray s -> Int -> Word -> ST s ()
writeByteArrayWord = PM.writeByteArray

writeByteArrayWord8 :: MutableByteArray s -> Int -> Word8 -> ST s ()
writeByteArrayWord8 = PM.writeByteArray

-- this is only used internally
unsafeIndexWord :: ByteArray -> Int -> Word
unsafeIndexWord = PM.indexByteArray

-- | Does not check to see if the index is in bounds.
unsafeIndex :: ByteArray -> Int -> Word8
unsafeIndex = PM.indexByteArray

zipAnd :: ByteArray -> ByteArray -> ByteArray
zipAnd x y = BAW.zipAnd 0 (length x) 0 (length y) x y

zipOr :: ByteArray -> ByteArray -> ByteArray
zipOr x y = BAW.zipOr 0 (length x) 0 (length y) x y

zipXor :: ByteArray -> ByteArray -> ByteArray
zipXor x y = BAW.zipXor 0 (length x) 0 (length y) x y

isAscii :: ByteArray -> Bool
isAscii x = BAW.isAscii 0 (length x) x

hash ::
     Int -- ^ buckets
  -> ByteArray -- ^ array
  -> Int
hash = hashWith 0

hashWith ::
     Int -- ^ salt
  -> Int -- ^ buckets
  -> ByteArray -- ^ array
  -> Int
hashWith salt buckets arr =
  BAW.hashWith 0 (length arr) salt buckets arr

withBytePtr ::
     PM.MutableByteArray RealWorld
  -> (PM.Addr -> IO b)
  -> IO b
withBytePtr marr@(PM.MutableByteArray marr#) f =
  case isMutableByteArrayPinned# marr# of
    1# -> do
      let !addr = PM.mutableByteArrayContents marr
      f addr
    _ -> do
      n <- PM.getSizeofMutableByteArray marr
      FMA.allocaBytes n $ \ptr -> do
        let !addr = ptrToAddr ptr
        !r <- f addr
        PM.copyAddrToByteArray marr 0 addr n
        return r

hGet :: Handle -> Int -> IO ByteArray
hGet h n
  | n > 0 = do
      (arr,_) <- hGetUnsafe h n 
      return arr
  | otherwise = return empty

-- Not exported. Precondition is that the requested size
-- must be greater than zero. Returns the bytearray along
-- with its size.
hGetUnsafe :: Handle -> Int -> IO (ByteArray,Int)
{-# INLINE hGetUnsafe #-}
hGetUnsafe h n = do
  !marr <- PM.newByteArray n
  !receivedByteCount <- withBytePtr marr $ \addr -> SIO.hGetBuf h (addrToPtr addr) n
  shrinkMutableByteArray marr receivedByteCount
  arr <- PM.unsafeFreezeByteArray marr
  return (arr,receivedByteCount)

-- | Like 'hGet', except that a shorter 'ByteString' may be returned
-- if there are not enough bytes immediately available to satisfy the
-- whole request.  'hGetSome' only blocks if there is no data
-- available, and EOF has not yet been reached.
hGetSome :: Int -> Handle -> IO ByteArray
hGetSome !n !h = do
  !marr <- PM.newByteArray n
  !receivedByteCount <- withBytePtr marr $ \addr -> SIO.hGetBufSome h (addrToPtr addr) n
  shrinkMutableByteArray marr receivedByteCount
  PM.unsafeFreezeByteArray marr

readFile :: FilePath -> IO ByteArray
readFile fp = SIO.withBinaryFile fp SIO.ReadMode $ \h -> do
  fileSize <- catch (hFileSize h) useZeroIfNotRegularFile
  let sz :: Int
      sz = fromIntegral fileSize `max` 255
  hGetContentsSizeHint h sz

useZeroIfNotRegularFile :: IOException -> IO Integer
useZeroIfNotRegularFile _ = return 0

-- Not exported. Precondition is that the initial read
-- size must be greater than zero.
hGetContentsSizeHint ::
     Handle -- handle
  -> Int -- initial read size, must be greater than zero
  -> IO ByteArray
hGetContentsSizeHint hnd = go LU.Nil 0 where
  go :: LU.List ByteArray# -> Int -> Int -> IO ByteArray
  go !chunks !totalSz !sz = do
    (ByteArray arr#,receivedByteCount) <- hGetUnsafe hnd sz
    -- We rely on the hGetBuf behaviour (not hGetBufSome) where it reads up
    -- to the size we ask for, or EOF. So short reads indicate EOF.
    if receivedByteCount < sz
      then reverseConcatByteArrayList (LU.Cons arr# chunks) (receivedByteCount + totalSz)
      else go (LU.Cons arr# chunks) (receivedByteCount + totalSz) ((sz+sz) `min` 32752)
           -- we grow the buffer sizes, but not too huge
           -- we concatenate in the end anyway

-- Not exported. Precondition is that the size argument must
-- be the total size of all of the bytearrays summed. This
-- does not really need to run in IO, but it is convenient to
-- do so here.
reverseConcatByteArrayList :: LU.List ByteArray# -> Int -> IO ByteArray
reverseConcatByteArrayList arrs sz = do
  !marr <- PM.newByteArray sz
  let go !_ LU.Nil = return ()
      go !pos (LU.Cons x xs) = do
        let chunkSize = PM.sizeofByteArray (PM.ByteArray x)
            posNext = pos - chunkSize
        PM.copyByteArray marr posNext (PM.ByteArray x) 0 chunkSize
        go posNext xs
  go sz arrs
  PM.unsafeFreezeByteArray marr

shrinkMutableByteArray :: PrimMonad m
  => MutableByteArray (PrimState m)
  -> Int -- ^ new size
  -> m ()
{-# INLINE shrinkMutableByteArray #-}
shrinkMutableByteArray (PM.MutableByteArray arr#) (I# n#)
  = primitive_ (shrinkMutableByteArray# arr# n#)

addrToPtr :: PM.Addr -> Ptr a
addrToPtr (PM.Addr x) = Ptr x

ptrToAddr :: Ptr a -> PM.Addr
ptrToAddr (Ptr x) = PM.Addr x

bigEndianWord16 :: Word16 -> ByteArray
bigEndianWord16 w = createByteArray 2 $ \m -> do
  PM.writeByteArray m 0 (selectOctet 0 w)
  PM.writeByteArray m 1 (selectOctet 1 w)

bigEndianWord32 :: Word32 -> ByteArray
bigEndianWord32 w = createByteArray 4 $ \m -> do
  PM.writeByteArray m 0 (selectOctet 0 w)
  PM.writeByteArray m 1 (selectOctet 1 w)
  PM.writeByteArray m 2 (selectOctet 2 w)
  PM.writeByteArray m 3 (selectOctet 3 w)

littleEndianWord32 :: Word32 -> ByteArray
littleEndianWord32 w = createByteArray 4 $ \m -> do
  PM.writeByteArray m 0 (selectOctet 3 w)
  PM.writeByteArray m 1 (selectOctet 2 w)
  PM.writeByteArray m 2 (selectOctet 1 w)
  PM.writeByteArray m 3 (selectOctet 0 w)

{-# INLINE selectOctet #-}
selectOctet :: forall w. (FiniteBits w, Integral w) => Int -> w -> Word8
selectOctet i w = fromIntegral (unsafeShiftR w ((finiteBitSize (undefined :: w) - 8) - i * 8))

createByteArray
  :: Int
  -> (forall s. MutableByteArray s -> ST s ())
  -> ByteArray
createByteArray n f = runArray $ do
  mary <- PM.newByteArray n
  f mary
  pure mary

runArray :: (forall s. ST s (MutableByteArray s)) -> ByteArray
runArray m = runST (m >>= PM.unsafeFreezeByteArray)


