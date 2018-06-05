{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}

{-# OPTIONS_GHC
 -Weverything
 -fno-warn-unsafe
 -fno-warn-implicit-prelude
 -fno-warn-missing-import-lists
 -O2
#-}

module Packed.Bytes.Builder
  ( Builder(..)
  , toByteArray
  ) where

import Control.Monad.ST (runST)
import Data.Primitive (ByteArray(..),MutableByteArray(..))

import GHC.Int (Int(I#))
import GHC.ST (ST(..))
import GHC.Magic (runRW#)
import GHC.Exts (State#,MutableByteArray#,Int#)
import GHC.Exts (copyMutableByteArray#,newByteArray#,unsafeFreezeByteArray#,shrinkMutableByteArray#,getSizeofMutableByteArray#)
import GHC.Exts ((+#),(>#),(*#))

import qualified Data.Foldable as F
import qualified Data.Primitive as PM

data Buffer s = Buffer !(MutableByteArray s) !Int

type Buffer# s = (# MutableByteArray# s, Int# #)

newtype Builder = Builder
  (forall s.
       (Buffer# s -> Int# -> State# s -> (# State# s, Buffer# s #))
       -- Callback that consumes a filled buffer. It accepts a size hint, and
       -- it returns a newly allocated buffer with length at least the size given
       -- by the hint. Do not reused the filled buffer after passing it to
       -- this callback.
    -> Buffer# s
    -> State# s -> (# State# s, Buffer# s #)
  )

instance Semigroup Builder where
  (<>) = append

instance Monoid Builder where
  mempty = empty

-- | Convert a builder to a byte array.
toByteArray :: Builder -> ByteArray
toByteArray = toByteArrayWith (2048 - 2 * PM.sizeOf (undefined :: Int))

-- | A variant of 'toByteArray' that gives the user the ability the specify
-- the initial buffer size.
toByteArrayWith ::
     Int -- ^ initial buffer size
  -> Builder -- ^ builder to serialize
  -> ByteArray
toByteArrayWith (I# initSz) (Builder f) = ByteArray
  ( runRW#
    ( \s0 -> case newByteArray# initSz s0 of
      (# s1, marr0 #) -> case f doublingBufferCallback (# marr0, 0# #) s1 of
        (# s2, (# marr1, n #) #) -> case shrinkMutableByteArray# marr1 n s2 of
          s3 -> case unsafeFreezeByteArray# marr1 s3 of
            (# _, arr #) -> arr
    )
  )

data FoldState s b = FoldState {-# UNPACK #-} !(Buffer s) !b

unboxInt :: Int -> Int#
unboxInt (I# i) = i

unboxMutableByteArray :: MutableByteArray s -> MutableByteArray# s
unboxMutableByteArray (MutableByteArray marr) = marr

unboxBuffer :: Buffer s -> Buffer# s
unboxBuffer (Buffer marr ix) = (# unboxMutableByteArray marr, unboxInt ix #)

boxBuffer :: Buffer# s -> Buffer s
boxBuffer (# marr, ix #) = Buffer (MutableByteArray marr) (I# ix)

-- | Strict left fold over the elements in the collection. Additionally,
-- the fold serializes the builders produced as the collection is passed
-- over.
foldlToByteArrayWith' :: forall f a b. Foldable f
  => Int -- ^ initial buffer size
  -> (b -> a -> (Builder,b))
  -> b
  -> f a
  -> (ByteArray,b)
foldlToByteArrayWith' n f b0 xs = runST $ do
  marr0 <- PM.newByteArray n
  FoldState (Buffer marr1 len) bFinal <- F.foldlM f' (FoldState (Buffer marr0 0) b0) xs
  marr2 <- PM.resizeMutableByteArray marr1 len
  arr <- PM.unsafeFreezeByteArray marr2
  return (arr,bFinal)
  where
  f' :: forall s. FoldState s b -> a -> ST s (FoldState s b)
  f' (FoldState buf1 b1) a = do
    let (theBuilder,b2) = f b1 a
    buf2 <- ST $ \s -> case intoBuffer (unboxBuffer buf1) theBuilder s of
      (# s', x #) -> (# s', boxBuffer x #)
    return (FoldState buf2 b2)

-- Consumes the buffer linearly. Do not reuse the argument buffer after
-- feeding it to this function.
intoBuffer :: Buffer# s -> Builder -> State# s -> (# State# s, Buffer# s #)
intoBuffer (# marr0, ix0 #) (Builder f) s0 = f doublingBufferCallback (# marr0, ix0 #) s0

doublingBufferCallback :: Buffer# s -> Int# -> State# s -> (# State# s, Buffer# s #)
doublingBufferCallback (# marr0, ix #) needed s0 = case getSizeofMutableByteArray# marr0 s0 of
  (# s1, sz #) -> case newByteArray# (max# (sz *# 2#) (ix +# needed)) s1 of
    (# s2, marr1 #) -> case copyMutableByteArray# marr0 0# marr1 0# sz s2 of
      s3 -> (# s3, (# marr1, ix #) #)

max# :: Int# -> Int# -> Int#
max# a b = case a ># b of
  1# -> a
  _ -> b

-- byte :: Word8 -> Builder
-- byte (W# w) = Builder 

append :: Builder -> Builder -> Builder
append (Builder f) (Builder g) = Builder
  ( \push (# arr0, ix0 #) s0 -> case f push (# arr0, ix0 #) s0 of
      (# s1, (# arr1, ix1 #) #) -> g push (# arr1, ix1 #) s1
  )

empty :: Builder
empty = Builder
  ( \_ (# arr0, ix0 #) s0 -> (# s0, (# arr0, ix0 #) #) )

