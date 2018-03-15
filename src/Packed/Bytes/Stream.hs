{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnboxedSums #-}

{-# OPTIONS_GHC -O2 #-}

module Packed.Bytes.Stream
  ( ByteStream(..)
  , empty
  , fromHandle
  ) where

import qualified Packed.Bytes as B
import qualified Data.Primitive as PM
import GHC.Exts (RealWorld,State#,Int#,ByteArray#)
import System.IO (Handle)
import GHC.Int (Int(I#))
import GHC.IO (IO(..))
import Packed.Bytes.Small (ByteArray(..))
import Packed.Bytes (Bytes(..))

type Bytes# = (# ByteArray#, Int#, Int# #)

newtype ByteStream s = ByteStream (State# s -> (# State# s, (# (# #) | (# Bytes# , ByteStream s #) #) #) )

empty :: ByteStream s
empty = ByteStream (\s -> (# s, (# (# #) | #) #) )

actionToByteStream :: ByteStream RealWorld -> IO (Maybe Bytes) -> ByteStream RealWorld
actionToByteStream stream (IO f) = ByteStream $ \s0 -> case f s0 of
  (# s1, m #) -> case m of
    Nothing -> (# s1, (# (# #) | #) #)
    Just b -> (# s1, (# | (# unpackBytes b, stream #) #) #)

packBytes :: Bytes# -> Bytes
packBytes (# a, b, c #) = Bytes (ByteArray a) (I# b) (I# c)

unpackBytes :: Bytes -> Bytes#
unpackBytes (Bytes (ByteArray a) (I# b) (I# c)) = (# a,b,c #)

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

