{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnboxedSums #-}

{-# OPTIONS_GHC -O2 #-}

module Packed.Bytes.Stream.IO
  ( ByteStream(..)
  , empty
  , singleton
  , toBytes
  , unpack
  , fromBytes
  , fromArray
  , fromHandle
  ) where

import Data.Word (Word8)
import GHC.Exts (RealWorld,State#,Int#,ByteArray#)
import GHC.Int (Int(I#))
import GHC.IO (IO(..))
import GHC.ST (ST(..))
import Packed.Bytes (Bytes(..))
import Packed.Bytes.Stream.ST (ByteStream(..))
import Data.Primitive (Array,ByteArray(..))
import System.IO (Handle)
import qualified Packed.Bytes as B
import qualified Packed.Bytes.Stream.ST as S
import qualified Data.Primitive as PM

type Bytes# = (# ByteArray#, Int#, Int# #)

empty :: ByteStream RealWorld
empty = S.empty

singleton :: Word8 -> ByteStream RealWorld
singleton = S.singleton

toBytes :: ByteStream RealWorld -> IO Bytes
toBytes = stToIO . S.toBytes

unpack :: ByteStream RealWorld -> IO [Word8]
unpack = stToIO . S.unpack

fromBytes :: Bytes -> ByteStream RealWorld
fromBytes = S.fromBytes

fromArray :: Array Bytes -> ByteStream RealWorld
fromArray = S.fromArray

-- copied from base's internal GHC.IO module
ioToST :: IO a -> ST RealWorld a
ioToST (IO m) = (ST m)

-- copied from base's internal GHC.IO module
stToIO :: ST RealWorld a -> IO a
stToIO (ST m) = IO m

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

actionToByteStream :: ByteStream RealWorld -> IO (Maybe Bytes) -> ByteStream RealWorld
actionToByteStream stream (IO f) = ByteStream $ \s0 -> case f s0 of
  (# s1, m #) -> case m of
    Nothing -> (# s1, (# (# #) | #) #)
    Just b -> (# s1, (# | (# unboxBytes b, stream #) #) #)


defaultChunkSize :: Int
defaultChunkSize = 32 * k - chunkOverhead
   where k = 1024

unboxBytes :: Bytes -> Bytes#
unboxBytes (Bytes (ByteArray a) (I# b) (I# c)) = (# a,b,c #)

