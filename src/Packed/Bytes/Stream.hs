{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnboxedSums #-}

{-# OPTIONS_GHC -O2 #-}

module Packed.Bytes.Stream
  ( ByteStream(..)
  , empty
  , singleton
  , unpack
  , unpackST
  , fromHandle
  ) where

import Data.Semigroup (Semigroup)
import Data.Word (Word8)
import GHC.Exts (RealWorld,State#,Int#,ByteArray#)
import GHC.IO (IO(..))
import GHC.Int (Int(I#))
import Packed.Bytes (Bytes(..))
import Packed.Bytes.Small (ByteArray(..))
import System.IO (Handle)
import GHC.ST (ST(..))
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
singleton w = ByteStream
  (\s0 -> (# s0, (# | (# unboxBytes (B.singleton w), empty #) #) #))
  
append :: ByteStream s -> ByteStream s -> ByteStream s
append (ByteStream f) x@(ByteStream g) = ByteStream $ \s0 -> case f s0 of
  (# s1, r #) -> case r of
    (# (# #) | #) -> g s1
    (# | (# bytes, stream #) #) -> (# s1, (# | (# bytes, append stream x #) #) #)

empty :: ByteStream s
empty = ByteStream (\s -> (# s, (# (# #) | #) #) )

actionToByteStream :: ByteStream RealWorld -> IO (Maybe Bytes) -> ByteStream RealWorld
actionToByteStream stream (IO f) = ByteStream $ \s0 -> case f s0 of
  (# s1, m #) -> case m of
    Nothing -> (# s1, (# (# #) | #) #)
    Just b -> (# s1, (# | (# unboxBytes b, stream #) #) #)

boxBytes :: Bytes# -> Bytes
boxBytes (# a, b, c #) = Bytes (ByteArray a) (I# b) (I# c)

unboxBytes :: Bytes -> Bytes#
unboxBytes (Bytes (ByteArray a) (I# b) (I# c)) = (# a,b,c #)

fromHandle :: Handle -> ByteStream RealWorld
fromHandle h = go where
  go :: ByteStream RealWorld
  go = actionToByteStream go $ do
    chunk <- B.hGetSome defaultChunkSize h
    return $ if B.null chunk
      then Nothing
      else Just chunk

chunkOverhead :: Int
chunkOverhead = 2 * PM.sizeOf (undefined :: Int)

defaultChunkSize :: Int
defaultChunkSize = 32 * k - chunkOverhead
   where k = 1024

unpackInternal :: ByteStream s -> State# s -> (# State# s, [Word8] #)
unpackInternal (ByteStream f) s0 = case f s0 of
  (# s1, r #) -> case r of
    (# (# #) | #) -> (# s1, [] #)
    (# | (# bytes, stream #) #) -> case unpackInternal stream s1 of
      (# s2, ws #) -> (# s2, B.unpack (boxBytes bytes) ++ ws #)

unpack :: ByteStream RealWorld -> IO [Word8]
unpack stream = IO (unpackInternal stream)

unpackST :: ByteStream s -> ST s [Word8]
unpackST stream = ST (unpackInternal stream)

