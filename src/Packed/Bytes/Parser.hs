{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
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

module Packed.Bytes.Parser
  ( Parser(..)
  , ParserLevity(..)
  , Result(..)
  , Leftovers(..)
  , parseStreamST
  , decimalWord
  ) where

import GHC.Int (Int(I#))
import GHC.Exts (State#,Int#,ByteArray#,Word#,(+#),(-#),plusWord#,timesWord#)
import GHC.Types (TYPE,RuntimeRep(..))
import GHC.Word (Word(W#))
import GHC.ST (ST(..))
import Packed.Bytes (Bytes(..))
import Packed.Bytes.Small (ByteArray(..))
import Data.Word (Word8)
import Packed.Bytes.Stream (ByteStream(..))
import qualified Packed.Bytes.Small as BA
import qualified Packed.Bytes.Stream as Stream

type Bytes# = (# ByteArray#, Int#, Int# #)
type Maybe# (a :: TYPE r) = (# (# #) | a #)
type Leftovers# s = (# ByteStream s, Bytes# #)
type Result# s (r :: RuntimeRep) (a :: TYPE r) =
  (# Maybe# (Leftovers# s), Maybe# a #)

data Result s a = Result
  { resultLeftovers :: !(Maybe (Leftovers s))
  , resultValue :: !(Maybe a)
  }

data Leftovers s = Leftovers
  { leftoversChunk :: {-# UNPACK #-} !Bytes
    -- ^ The last chunk pulled from the stream
  , leftoversStream :: ByteStream s
    -- ^ The remaining stream
  }

parseStreamST :: ByteStream s -> Parser a -> ST s (Result s a)
parseStreamST stream (Parser (ParserLevity f)) = ST $ \s0 ->
  case f (# | (# stream, (# unboxByteArray BA.empty, 0#, 0# #) #) #) s0 of
    (# s1, r #) -> (# s1, boxResult r #)

boxResult :: Result# s LiftedRep a -> Result s a
boxResult (# leftovers, val #) = case val of
  (# (# #) | #) -> Result (boxLeftovers leftovers) Nothing
  (# | a #) -> Result (boxLeftovers leftovers) (Just a)

boxLeftovers :: Maybe# (Leftovers# s) -> Maybe (Leftovers s)
boxLeftovers (# (# #) | #) = Nothing
boxLeftovers (# | (# stream, bytes #) #) = Just (Leftovers (boxBytes bytes) stream)

newtype Parser a = Parser (ParserLevity LiftedRep a)

newtype ParserLevity (r :: RuntimeRep) (a :: TYPE r) = ParserLevity
  { getParserLevity :: forall s.
       Maybe# (Leftovers# s)
    -> State# s
    -> (# State# s, Result# s r a #)
  }

bytesLength :: Bytes# -> Int
bytesLength (# _, _, len #) = I# len

bytesPayload :: Bytes# -> ByteArray
bytesPayload (# arr, _, _ #) = ByteArray arr

bytesIndex :: Bytes# -> Int -> Word8
bytesIndex (# arr, off, _ #) ix = BA.unsafeIndex (ByteArray arr) (I# off + ix)

word8ToWord :: Word8 -> Word
word8ToWord = fromIntegral

nextNonEmpty :: ByteStream s -> State# s -> (# State# s, Maybe# (Leftovers# s) #)
nextNonEmpty (ByteStream f) s0 = case f s0 of
  (# s1, r #) -> case r of
    (# (# #) | #) -> (# s1, (# (# #) | #) #)
    (# | (# bytes@(# _,_,len #), stream #) #) -> case len of
      0# -> nextNonEmpty stream s1
      _ -> (# s1, (# | (# stream, bytes #) #) #)

decimalDigit :: ParserLevity WordRep Word#
decimalDigit = ParserLevity $ \leftovers0 s0 -> case leftovers0 of
  (# (# #) | #) -> (# s0, (# (# (# #) | #), (# (# #) | #) #) #)
  (# | (# stream0, bytes0@(# _,_,len #) #) #) ->
    let !(# s1, r #) = case len of
          0# -> nextNonEmpty stream0 s0
          _ -> (# s0, (# | (# stream0, bytes0 #) #) #)
     in case r of
          (# (# #) | #) -> (# s1, (# (# (# #) | #), (# (# #) | #) #) #)
          (# | (# stream1, bytes1 #) #) ->
            let !w = word8ToWord (bytesIndex bytes1 0) - 48
             in if w < 10
                  then (# s1, (# (# | (# stream1, unsafeDrop# 1 bytes1 #) #), (# | unboxWord w #) #) #)
                  else (# s1, (# (# | (# stream1, bytes1 #) #), (# (# #) | #) #) #)

decimalContinue :: Word# -> ParserLevity WordRep Word#
decimalContinue theWord = ParserLevity (action theWord) where
  action :: Word# -> Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# s WordRep Word# #)
  action !w0 (# (# #) | #) s0 = (# s0, (# (# (# #) | #), (# | w0 #) #) #)
  action !w0 (# | leftovers0 #) s0 = go w0 leftovers0 s0
  go :: Word# -> Leftovers# s -> State# s -> (# State# s, Result# s WordRep Word# #)
  go !w0 leftovers0@(# !stream0, !bytes0@(# _,_,len #) #) s0 = 
    let !(# s1, leftovers1 #) = case len of
          0# -> nextNonEmpty stream0 s0
          _ -> (# s0, (# | leftovers0 #) #)
     in case leftovers1 of
          (# (# #) | #) -> (# s1, (# (# (# #) | #), (# | w0 #) #) #)
          (# | (# stream1, bytes1 #) #) ->
            let !w = word8ToWord (bytesIndex bytes1 0) - 48
             in if w < 10
                  then go (plusWord# (timesWord# w0 10##) (unboxWord w)) (# stream1, bytes1 #) s1
                  else (# s1, (# (# | (# stream1, bytes1 #) #), (# | w0 #) #) #)
         
decimalWordUnboxed :: ParserLevity WordRep Word#
decimalWordUnboxed = bindWord decimalDigit decimalContinue

decimalWord :: Parser Word
decimalWord = Parser (boxWordParser decimalWordUnboxed)

bindWord :: ParserLevity WordRep Word# -> (Word# -> ParserLevity WordRep Word#) -> ParserLevity WordRep Word#
bindWord (ParserLevity f) g = ParserLevity $ \leftovers0 s0 -> case f leftovers0 s0 of
  (# s1, (# leftovers1, val #) #) -> case val of
    (# (# #) | #) -> (# s1, (# leftovers1, (# (# #) | #) #) #)
    (# | x #) -> case g x of
      ParserLevity k -> k leftovers1 s1

boxWordParser :: ParserLevity WordRep Word# -> ParserLevity LiftedRep Word
boxWordParser p = ParserLevity $ \leftovers0 s0 ->
  case getParserLevity p leftovers0 s0 of
    (# s1, (# leftovers1, val #) #) -> case val of
      (# (# #) | #) -> (# s1, (# leftovers1, (# (# #) | #) #) #)
      (# | x #) -> (# s1, (# leftovers1, (# | W# x #) #) #)

unboxWord :: Word -> Word#
unboxWord (W# i) = i

-- This assumes that the Bytes is longer than the index. It also does
-- not eliminate zero-length references to byte arrays.
unsafeDrop# :: Int -> Bytes# -> Bytes#
unsafeDrop# (I# i) (# arr, off, len #) = (# arr, off +# i, len -# i #)

unboxByteArray :: ByteArray -> ByteArray#
unboxByteArray (ByteArray arr) = arr

boxBytes :: Bytes# -> Bytes
boxBytes (# a, b, c #) = Bytes (ByteArray a) (I# b) (I# c)

