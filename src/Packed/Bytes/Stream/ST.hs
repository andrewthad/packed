{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnboxedSums #-}

{-# OPTIONS_GHC -O2 #-}

module Packed.Bytes.Stream.ST
  ( ByteStream(..)
  , empty
  , singleton
  , toBytes
  , unpack
  , fromBytes
  , fromArray
  ) where

import Data.Primitive (Array)
import Data.Semigroup (Semigroup)
import Data.Word (Word8)
import GHC.Exts (RealWorld,State#,Int#,ByteArray#)
import GHC.Int (Int(I#))
import GHC.ST (ST(..))
import Packed.Bytes (Bytes(..))
import Packed.Bytes.Small (ByteArray(..))
import System.IO (Handle)
import qualified Data.Primitive as PM
import qualified Data.Semigroup as SG
import qualified Packed.Bytes as B

type Bytes# = (# ByteArray#, Int#, Int# #)

newtype ByteStream s = ByteStream
  (State# s -> (# State# s, (# (# #) | (# Bytes# , ByteStream s #) #) #) )

instance Semigroup (ByteStream s) where
  (<>) = append

instance Monoid (ByteStream s) where
  mempty = empty
  mappend = (SG.<>)

singleton :: Word8 -> ByteStream s
singleton !w = ByteStream
  (\s0 -> (# s0, (# | (# unboxBytes (B.singleton w), empty #) #) #))
  
fromBytes :: Bytes -> ByteStream s
fromBytes b = ByteStream
  (\s0 -> (# s0, (# | (# unboxBytes b, empty #) #) #))

toBytes :: ByteStream s -> ST s Bytes
toBytes = fmap mconcat . go where
  go :: ByteStream s -> ST s [Bytes]
  go s0 = nextChunk s0 >>= \case
    Nothing -> return []
    Just (b,s1) -> fmap (b:) (go s1)

nextChunk :: ByteStream s -> ST s (Maybe (Bytes,ByteStream s))
nextChunk (ByteStream f) = ST $ \s0 -> case f s0 of
  (# s1, r #) -> case r of
    (# (# #) | #) -> (# s1, Nothing #)
    (# | (# theBytes, theStream #) #) -> (# s1, Just (boxBytes theBytes, theStream) #)

fromArray :: forall s. Array Bytes -> ByteStream s
fromArray xs = ByteStream (go 0) where
  !len = PM.sizeofArray xs
  go :: Int -> State# s -> (# State# s, (# (# #) | (# Bytes# , ByteStream s #) #) #)
  go !ix s0 = if ix < len
    then
      let !x = unboxBytes (PM.indexArray xs ix)
       in (# s0, (# | (# x, ByteStream (go (ix + 1)) #) #) #)
    else (# s0, (# (# #) | #) #)
  
append :: ByteStream s -> ByteStream s -> ByteStream s
append (ByteStream f) x@(ByteStream g) = ByteStream $ \s0 -> case f s0 of
  (# s1, r #) -> case r of
    (# (# #) | #) -> g s1
    (# | (# bytes, stream #) #) -> (# s1, (# | (# bytes, append stream x #) #) #)

empty :: ByteStream s
empty = ByteStream (\s -> (# s, (# (# #) | #) #) )

boxBytes :: Bytes# -> Bytes
boxBytes (# a, b, c #) = Bytes (ByteArray a) (I# b) (I# c)

unboxBytes :: Bytes -> Bytes#
unboxBytes (Bytes (ByteArray a) (I# b) (I# c)) = (# a,b,c #)

unpack :: ByteStream s -> ST s [Word8]
unpack stream = ST (unpackInternal stream)

unpackInternal :: ByteStream s -> State# s -> (# State# s, [Word8] #)
unpackInternal (ByteStream f) s0 = case f s0 of
  (# s1, r #) -> case r of
    (# (# #) | #) -> (# s1, [] #)
    (# | (# bytes, stream #) #) -> case unpackInternal stream s1 of
      (# s2, ws #) -> (# s2, B.unpack (boxBytes bytes) ++ ws #)
