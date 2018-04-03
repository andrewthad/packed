{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
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

module Packed.Bytes.Parser
  ( Parser(..)
  , ParserLevity(..)
  , Result(..)
  , PureResult(..)
  , Leftovers(..)
  , parseBytes
  , parseStreamST
  , parseStreamIO
  , decimalWord
  , takeBytesWhileMember
  , takeBytesUntilMemberConsume
  , takeBytesUntilByteConsume
  , takeBytesUntilByte
  , skipUntilByteConsume
  , skipUntilByte
  , skipDigits
  , bytes
  , byte
  , any
  , endOfInput
  , isEndOfInput
  , replicate
  , replicateUntilEnd
  -- , replicateIntersperseUntilEnd
  -- , replicateUntilByte
  , replicateUntilMember
  , foldlIntersperseParserUntilEnd
  , replicate
  , failure
    -- * ASCII
  , skipSpace
  , endOfLine
  , takeBytesUntilEndOfLineConsume
  ) where

import Prelude hiding (any,replicate)
import GHC.Int (Int(I#))
import GHC.Exts (State#,Int#,ByteArray#,Word#,(+#),(-#),(>#),(<#),
  MutableArray#,writeArray#,unsafeFreezeArray#,newArray#,
  unsafeFreezeByteArray#,newByteArray#,runRW#,
  plusWord#,timesWord#,indexWord8Array#,eqWord#,fromListN,
  RealWorld)
import GHC.Types (TYPE,RuntimeRep(..),IO(..))
import GHC.Word (Word(W#),Word8(W8#))
import GHC.ST (ST(..))
import Packed.Bytes (Bytes(..))
import Packed.Bytes.Small (ByteArray(..))
import Packed.Bytes.Stream.ST (ByteStream(..))
import Packed.Bytes.Set (ByteSet)
import Data.Primitive (Array(..),MutableArray(..))
import Control.Monad.ST (runST)
import qualified Control.Monad
import qualified Packed.Bytes.Small as BA
import qualified Packed.Bytes.Stream.ST as Stream
import qualified Packed.Bytes.Window as BAW
import qualified Packed.Bytes as B
import qualified Packed.Bytes.Set as ByteSet
import qualified Data.Primitive as PM

type Bytes# = (# ByteArray#, Int#, Int# #)
type Maybe# (a :: TYPE r) = (# (# #) | a #)
type Leftovers# s = (# Bytes# , ByteStream s #)
type Result# s (r :: RuntimeRep) (a :: TYPE r) =
  (# Maybe# (Leftovers# s), Maybe# a #)
type BytesRep = TupleRep '[ 'UnliftedRep, 'IntRep, 'IntRep ]

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

data PureResult a = PureResult
  { pureResultLeftovers :: {-# UNPACK #-} !Bytes
  , pureResultValue :: !(Maybe a)
  } deriving (Show,Eq)

parseBytes :: Bytes -> Parser a -> PureResult a
parseBytes bytes p = runST $ do
  Result mleftovers mval <- parseStreamST (Stream.fromBytes bytes) p
  theLeftovers <- case mleftovers of
    Nothing -> return B.empty
    Just (Leftovers chunk stream) -> do
      others <- Stream.toBytes stream
      return (B.append chunk others)
  return (PureResult theLeftovers mval)

parseStreamST :: ByteStream s -> Parser a -> ST s (Result s a)
parseStreamST stream (Parser (ParserLevity f)) = ST $ \s0 ->
  case f (# | (# (# unboxByteArray BA.empty, 0#, 0# #), stream #) #) s0 of
    (# s1, r #) -> (# s1, boxResult r #)

parseStreamIO :: ByteStream RealWorld -> Parser a -> IO (Result RealWorld a)
parseStreamIO stream (Parser (ParserLevity f)) = IO $ \s0 ->
  case f (# | (# (# unboxByteArray BA.empty, 0#, 0# #), stream #) #) s0 of
    (# s1, r #) -> (# s1, boxResult r #)

boxResult :: Result# s 'LiftedRep a -> Result s a
boxResult (# leftovers, val #) = case val of
  (# (# #) | #) -> Result (boxLeftovers leftovers) Nothing
  (# | a #) -> Result (boxLeftovers leftovers) (Just a)

boxLeftovers :: Maybe# (Leftovers# s) -> Maybe (Leftovers s)
boxLeftovers (# (# #) | #) = Nothing
boxLeftovers (# | (# theBytes, stream #) #) = Just (Leftovers (boxBytes theBytes) stream)

newtype Parser a = Parser (ParserLevity 'LiftedRep a)

instance Functor Parser where
  fmap = mapParser

instance Applicative Parser where
  pure = pureParser
  (<*>) = Control.Monad.ap

instance Monad Parser where
  return = pure
  (>>=) = bindLifted

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

-- option :: a -> Parser a -> Parser a
-- option a (

nextNonEmpty :: ByteStream s -> State# s -> (# State# s, Maybe# (Leftovers# s) #)
nextNonEmpty (ByteStream f) s0 = case f s0 of
  (# s1, r #) -> case r of
    (# (# #) | #) -> (# s1, (# (# #) | #) #)
    (# | (# theBytes@(# _,_,len #), stream #) #) -> case len of
      0# -> nextNonEmpty stream s1
      _ -> (# s1, (# | (# theBytes, stream #) #) #)

{-# INLINE withNonEmpty #-}
withNonEmpty :: forall s (r :: RuntimeRep) (b :: TYPE r).
     Maybe# (Leftovers# s)
  -> State# s
  -> (State# s -> (# State# s, Result# s r b #))
  -> (Word# -> Bytes# -> ByteStream s -> State# s -> (# State# s, Result# s r b #))
     -- The first argument is a Word8, not a full machine word.
     -- The second argument is the complete,non-empty chunk
     -- with the head byte still intact.
  -> (# State# s, Result# s r b #)
withNonEmpty (# (# #) | #) s0 g _ = g s0
withNonEmpty (# | (# bytes0@(# arr0,off0,len0 #), stream0 #) #) s0 g f = case len0 ># 0# of
  1# -> f (indexWord8Array# arr0 off0) bytes0 stream0 s0
  _ -> case nextNonEmpty stream0 s0 of
    (# s1, r #) -> case r of
      (# (# #) | #) -> g s1
      (# | (# bytes1@(# arr1, off1, _ #), stream1 #) #) -> 
        f (indexWord8Array# arr1 off1) bytes1 stream1 s1

decimalDigit :: ParserLevity 'WordRep Word#
decimalDigit = ParserLevity $ \leftovers0 s0 -> case leftovers0 of
  (# (# #) | #) -> (# s0, (# (# (# #) | #), (# (# #) | #) #) #)
  (# | (# bytes0@(# _,_,len #), stream0 #) #) ->
    let !(# s1, r #) = case len of
          0# -> nextNonEmpty stream0 s0
          _ -> (# s0, (# | (# bytes0, stream0 #) #) #)
     in case r of
          (# (# #) | #) -> (# s1, (# (# (# #) | #), (# (# #) | #) #) #)
          (# | (# bytes1, stream1 #) #) ->
            let !w = word8ToWord (bytesIndex bytes1 0) - 48
             in if w < 10
                  then (# s1, (# (# | (# unsafeDrop# 1 bytes1, stream1 #) #), (# | unboxWord w #) #) #)
                  else (# s1, (# (# | (# bytes1, stream1 #) #), (# (# #) | #) #) #)

decimalContinue :: Word# -> ParserLevity 'WordRep Word#
decimalContinue theWord = ParserLevity (action theWord) where
  action :: Word# -> Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# s 'WordRep Word# #)
  action !w0 (# (# #) | #) s0 = (# s0, (# (# (# #) | #), (# | w0 #) #) #)
  action !w0 (# | leftovers0 #) s0 = go w0 leftovers0 s0
  go :: Word# -> Leftovers# s -> State# s -> (# State# s, Result# s 'WordRep Word# #)
  go !w0 leftovers0@(# (# _,_,len #), !stream0 #) s0 = 
    let !(# s1, leftovers1 #) = case len of
          0# -> nextNonEmpty stream0 s0
          _ -> (# s0, (# | leftovers0 #) #)
     in case leftovers1 of
          (# (# #) | #) -> (# s1, (# (# (# #) | #), (# | w0 #) #) #)
          (# | (# bytes1, stream1 #) #) ->
            let !w = word8ToWord (bytesIndex bytes1 0) - 48
             in if w < 10
                  then go (plusWord# (timesWord# w0 10##) (unboxWord w)) (# unsafeDrop# 1 bytes1, stream1 #) s1
                  else (# s1, (# (# | (# bytes1, stream1 #) #), (# | w0 #) #) #)

decimalWordUnboxed :: ParserLevity 'WordRep Word#
decimalWordUnboxed = bindWord decimalDigit decimalContinue

skipSpaceUnboxed :: ParserLevity 'LiftedRep ()
skipSpaceUnboxed = ParserLevity go where
  go :: Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# s 'LiftedRep () #)
  go (# (# #) | #) s0 = (# s0, (# (# (# #) | #), (# | () #) #) #)
  go (# | (# bytes0@(# arr, off, len #), !stream0@(ByteStream streamFunc) #) #) s0 = case BAW.findNonAsciiSpace' (I# off) (I# len) (ByteArray arr) of
    (# (# #) | #) -> case streamFunc s0 of
      (# s1, r #) -> go r s1
    (# | ix #) -> (# s0, (# (# | (# unsafeDrop# (I# (ix -# off)) bytes0, stream0 #) #), (# | () #) #) #)

-- | Takes all 'Bytes' until the first occurrence of a byte in the
--   provided set. The bytes leading up to it are returned as the
--   first element of the tuple. The matching byte itself is consumed
--   from the input and returned as the second element of the tuple. This
--   parser will fail if the input ends before a byte from the set is
--   encountered.
{-# INLINE takeBytesUntilMemberConsume #-}
takeBytesUntilMemberConsume :: ByteSet -> Parser (Bytes,Word8)
takeBytesUntilMemberConsume (ByteSet.ByteSet (ByteArray set)) = Parser (boxBytesWord8Parser (takeBytesUntilMemberConsumeUnboxed set))

{-# NOINLINE takeBytesUntilMemberConsumeUnboxed #-}
takeBytesUntilMemberConsumeUnboxed :: ByteArray# -> ParserLevity ('TupleRep '[BytesRep, 'WordRep]) (# Bytes#, Word# #)
takeBytesUntilMemberConsumeUnboxed !set = ParserLevity (go (# (# #) | #)) where
  go :: Maybe# Bytes# -> Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# s ('TupleRep '[BytesRep, 'WordRep]) (# Bytes#, Word# #) #)
  go !mbytes (# (# #) | #) s0 = (# s0, (# (# (# #) | #), (# (# #) | #) #) #)
  go !mbytes (# | (# bytes0@(# arr, off, len #), !stream0@(ByteStream streamFunc) #) #) s0 = case BAW.findMemberByte (I# off) (I# len) (ByteSet.ByteSet (ByteArray set)) (ByteArray arr) of
    Nothing -> case streamFunc s0 of
      (# s1, r #) -> go (# | appendMaybeBytes mbytes bytes0 #) r s1
    Just (I# ix, W8# w) -> (# s0, (# (# | (# unsafeDrop# (I# ((ix -# off) +# 1# )) bytes0, stream0 #) #), (# | (# appendMaybeBytes mbytes (# arr, off, ix -# off #), w #) #) #) #)

-- | Takes all 'Bytes' until the first occurrence of the given byte.
--   The bytes leading up to it are returned, and the matching byte
--   itself is consumed from the input.
{-# INLINE takeBytesUntilByteConsume #-}
takeBytesUntilByteConsume :: Word8 -> Parser Bytes
takeBytesUntilByteConsume (W8# theByte) = Parser (boxBytesParser (takeBytesUntilByteConsumeUnboxed theByte))

{-# NOINLINE takeBytesUntilByteConsumeUnboxed #-}
takeBytesUntilByteConsumeUnboxed :: Word# -> ParserLevity BytesRep Bytes#
takeBytesUntilByteConsumeUnboxed !theByte = ParserLevity (go (# (# #) | #)) where
  go :: Maybe# Bytes# -> Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# s BytesRep Bytes# #)
  go !mbytes (# (# #) | #) s0 = (# s0, (# (# (# #) | #), (# (# #) | #) #) #)
  go !mbytes (# | (# bytes0@(# arr, off, len #), !stream0@(ByteStream streamFunc) #) #) s0 = case BAW.findByte (I# off) (I# len) (W8# theByte) (ByteArray arr) of
    Nothing -> case streamFunc s0 of
      (# s1, r #) -> go (# | appendMaybeBytes mbytes bytes0 #) r s1
    Just (I# ix) -> (# s0, (# (# | (# unsafeDrop# (I# ((ix -# off) +# 1# )) bytes0, stream0 #) #), (# | appendMaybeBytes mbytes (# arr, off, ix -# off #) #) #) #)

-- | Takes all 'Bytes' until the first occurrence of the given byte.
--   The bytes leading up to it are returned, and the matching byte
--   itself remains part of the input.
{-# INLINE takeBytesUntilByte #-}
takeBytesUntilByte :: Word8 -> Parser Bytes
takeBytesUntilByte (W8# theByte) = Parser (boxBytesParser (takeBytesUntilByteUnboxed theByte))

{-# NOINLINE takeBytesUntilByteUnboxed #-}
takeBytesUntilByteUnboxed :: Word# -> ParserLevity BytesRep Bytes#
-- fix this. It should succeed if it reaches the end of the input.
takeBytesUntilByteUnboxed !theByte = ParserLevity (go (# (# #) | #)) where
  go :: Maybe# Bytes# -> Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# s BytesRep Bytes# #)
  go !mbytes (# (# #) | #) s0 = (# s0, (# (# (# #) | #), (# (# #) | #) #) #)
  go !mbytes (# | (# bytes0@(# arr, off, len #), !stream0@(ByteStream streamFunc) #) #) s0 = case BAW.findByte (I# off) (I# len) (W8# theByte) (ByteArray arr) of
    Nothing -> case streamFunc s0 of
      (# s1, r #) -> go (# | appendMaybeBytes mbytes bytes0 #) r s1
    Just (I# ix) -> (# s0, (# (# | (# unsafeDrop# (I# (ix -# off)) bytes0, stream0 #) #), (# | appendMaybeBytes mbytes (# arr, off, ix -# off #) #) #) #)

skipDigits :: Parser ()
skipDigits = Parser (ParserLevity go) where
  go :: Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# s 'LiftedRep () #)
  go (# (# #) | #) s0 = (# s0, (# (# (# #) | #), (# | () #) #) #)
  go (# | (# bytes0@(# arr, off, len #), !stream0@(ByteStream streamFunc) #) #) s0 = case BAW.findNonDigit (I# off) (I# len) (ByteArray arr) of
    Nothing -> case streamFunc s0 of
      (# s1, r #) -> go r s1
    Just (I# ix, _) -> (# s0, (# (# | (# unsafeDrop# (I# (ix -# off)) bytes0, stream0 #) #), (# | () #) #) #)

skipUntilByteConsume :: Word8 -> Parser ()
skipUntilByteConsume (W8# w) = Parser (skipUntilByteConsumeUnboxed w)

skipUntilByteConsumeUnboxed :: Word# -> ParserLevity 'LiftedRep ()
skipUntilByteConsumeUnboxed !theByte = ParserLevity go where
  go :: Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# s 'LiftedRep () #)
  go (# (# #) | #) s0 = (# s0, (# (# (# #) | #), (# (# #) | #) #) #)
  go (# | (# bytes0@(# arr, off, len #), !stream0@(ByteStream streamFunc) #) #) s0 = case BAW.findByte (I# off) (I# len) (W8# theByte) (ByteArray arr) of
    Nothing -> case streamFunc s0 of
      (# s1, r #) -> go r s1
    Just (I# ix) -> (# s0, (# (# | (# unsafeDrop# (I# ((ix -# off) +# 1# )) bytes0, stream0 #) #), (# | () #) #) #)

skipUntilByte :: Word8 -> Parser ()
skipUntilByte (W8# theByte) = Parser (ParserLevity go) where
  go :: Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# s 'LiftedRep () #)
  go (# (# #) | #) s0 = (# s0, (# (# (# #) | #), (# | () #) #) #)
  go (# | (# bytes0@(# arr, off, len #), !stream0@(ByteStream streamFunc) #) #) s0 = case BAW.findByte (I# off) (I# len) (W8# theByte) (ByteArray arr) of
    Nothing -> case streamFunc s0 of
      (# s1, r #) -> go r s1
    Just (I# ix) -> (# s0, (# (# | (# unsafeDrop# (I# (ix -# off)) bytes0, stream0 #) #), (# | () #) #) #)

{-# INLINE takeBytesUntilEndOfLineConsume #-}
takeBytesUntilEndOfLineConsume :: Parser Bytes
takeBytesUntilEndOfLineConsume = Parser (boxBytesParser takeBytesUntilEndOfLineConsumeUnboxed)

{-# NOINLINE takeBytesUntilEndOfLineConsumeUnboxed #-}
takeBytesUntilEndOfLineConsumeUnboxed :: ParserLevity BytesRep Bytes#
takeBytesUntilEndOfLineConsumeUnboxed = ParserLevity (go (# (# #) | #)) where
  go :: Maybe# Bytes# -> Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# s BytesRep Bytes# #)
  go !mbytes (# (# #) | #) s0 = (# s0, (# (# (# #) | #), (# (# #) | #) #) #)
  go !mbytes (# | (# bytes0@(# arr0, off0, len0 #), !stream0@(ByteStream streamFunc) #) #) s0 = case BAW.findAnyByte2 (I# off0) (I# len0) 10 13 (ByteArray arr0) of
    Nothing -> case streamFunc s0 of
      (# s1, r #) -> go (# | appendMaybeBytes mbytes bytes0 #) r s1
    Just (I# ix, W8# theByte) -> case theByte of
      10## -> (# s0, (# (# | (# unsafeDrop# (I# ((ix -# off0) +# 1# )) bytes0, stream0 #) #), (# | appendMaybeBytes mbytes (# arr0, off0, ix -# off0 #) #) #) #)
      -- second case means it was 13
      _ -> case ix <# (off0 +# len0 -# 1#) of
        1# -> case indexWord8Array# arr0 (ix +# 1# ) of
          10## -> (# s0, (# (# | (# unsafeDrop# (I# ((ix -# off0) +# 2# )) bytes0, stream0 #) #), (# | appendMaybeBytes mbytes (# arr0, off0, ix -# off0 #) #) #) #)
          _ -> (# s0, (# (# | (# unsafeDrop# (I# (ix -# off0)) bytes0, stream0 #) #), (# (# #) | #) #) #)
        _ -> case nextNonEmpty stream0 s0 of
          (# s1, m #) -> case m of
            (# (# #) | #) -> (# s1, (# (# | (# unboxBytes (B.singleton 13), Stream.empty #) #), (# (# #) | #) #) #)
            (# | (# bytes1@(# arr1, off1, len1 #), stream1 #) #) -> case indexWord8Array# arr1 0# of
              10## -> (# s1, (# (# | (# unsafeDrop# (I# 1#) bytes1, stream1 #) #), (# | appendMaybeBytes mbytes (# arr0, off0, ix -# off0 #) #) #) #)
              _ -> (# s1, (# (# | (# unboxBytes (B.cons 13 (boxBytes bytes1)), stream1 #) #), (# (# #) | #) #) #)

-- | Takes all 'Bytes' until the first occurrence of a byte in the
--   provided set. The bytes leading up to it are returned. The matching
--   byte is not consumed. This parser succeeds if the input ends before
--   a byte from the set is encountered.
takeBytesWhileMember :: ByteSet -> Parser Bytes
takeBytesWhileMember b = Parser (takeBytesWhileMemberUnboxed b)

takeBytesWhileMemberUnboxed :: ByteSet -> ParserLevity 'LiftedRep Bytes
takeBytesWhileMemberUnboxed !set = ParserLevity (go (# (# #) | #)) where
  go :: Maybe# Bytes# -> Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# s 'LiftedRep Bytes #)
  go mbytes (# (# #) | #) s0 = (# s0, (# (# (# #) | #), (# | maybeBytesToBytes mbytes #) #) #)
  go mbytes (# | (# bytes0@(# arr, off, len #), !stream0@(ByteStream streamFunc) #) #) s0 = case BAW.findNonMemberByte (I# off) (I# len) set (ByteArray arr) of
    Nothing -> case streamFunc s0 of
      (# s1, r #) -> go (# | appendMaybeBytes mbytes bytes0 #) r s1
    Just (I# ix,!_) -> (# s0, (# (# | (# unsafeDrop# (I# (ix -# off)) bytes0, stream0 #) #), (# | boxBytes (appendMaybeBytes mbytes (# arr, off, ix -# off #)) #) #) #)

byteUnboxed :: Word8 -> ParserLevity 'LiftedRep ()
byteUnboxed expectedByte@(W8# expected) = ParserLevity go where
  go :: Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# s 'LiftedRep () #)
  go m s0 = withNonEmpty m s0
    (\s -> (# s, (# (# (# #) | #), (# (# #) | #) #) #))
    (\actual theBytes stream s -> case eqWord# expected actual of
      1# -> (# s, (# (# | (# unsafeDrop# 1 theBytes, stream #) #), (# | () #) #) #)
      _ -> (# s, (# (# | (# theBytes, stream #) #), (# (# #) | #) #) #)
    )

anyUnboxed :: ParserLevity 'WordRep Word#
anyUnboxed = ParserLevity go where
  go :: Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# s 'WordRep Word# #)
  go m s0 = withNonEmpty m s0
    (\s -> (# s, (# (# (# #) | #), (# (# #) | #) #) #))
    (\theByte theBytes stream s ->
      (# s, (# (# | (# unsafeDrop# 1 theBytes, stream #) #), (# | theByte #) #) #)
    )

isEndOfInput :: Parser Bool
isEndOfInput = Parser (ParserLevity go) where
  go :: Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# s 'LiftedRep Bool #)
  go m s0 = withNonEmpty m s0
    (\s -> (# s, (# (# (# #) | #), (# | True #) #) #))
    (\_ theBytes stream s -> 
      (# s, (# (# | (# theBytes, stream #) #), (# | False #) #) #)
    )

endOfInputUnboxed :: ParserLevity 'LiftedRep ()
endOfInputUnboxed = ParserLevity go where
  go :: Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# s 'LiftedRep () #)
  go m s0 = withNonEmpty m s0
    (\s -> (# s, (# (# (# #) | #), (# | () #) #) #))
    (\_ theBytes stream s -> 
      (# s, (# (# | (# theBytes, stream #) #), (# (# #) | #) #) #)
    )

endOfInput :: Parser ()
endOfInput = Parser endOfInputUnboxed

any :: Parser Word8
any = Parser (boxWord8Parser anyUnboxed)

byte :: Word8 -> Parser ()
byte theByte = Parser (byteUnboxed theByte)

bytes :: Bytes -> Parser ()
bytes b = Parser (bytesUnboxed b)

bytesUnboxed :: Bytes -> ParserLevity 'LiftedRep ()
bytesUnboxed !theBytes@(Bytes parr poff plen) = ParserLevity (go poff) where
  pend = poff + plen
  go :: Int -> Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# s 'LiftedRep () #)
  go !ix (# (# #) | #) s0 = if ix == pend
    then (# s0, (# (# (# #) | #), (# | () #) #) #)
    else (# s0, (# (# (# #) | #), (# (# #) | #) #) #)
  go !ix (# | leftovers@(# bytes0@(# arr, off, len #), !stream0@(ByteStream streamFunc) #) #) s0 = 
    case BAW.stripPrefixResumable ix (I# off) (pend - ix) (I# len) parr (ByteArray arr) of
      (# (# #) | | #) -> (# s0, (# (# | leftovers #), (# (# #) | #) #) #)
      (# | (# #) | #) -> (# s0, (# (# | (# unsafeDrop# (pend - ix) bytes0, stream0 #) #), (# | () #) #) #)
      (# | | (# #) #) -> case streamFunc s0 of
        (# s1, r #) -> go (ix + I# len) r s1

-- replicateUntilMember :: forall a. ByteSet -> Parser a -> Parser (Array a)
-- replicateUntilMember separators b p = go []
--   where
--   go :: [a] -> Parser
--   go !xs = 
  
-- | Repeat the parser a specified number of times. The implementation
--   is tail recursive and avoids building up a list as an intermediate
--   data structure. Instead, it writes to an array directly. 
replicate :: forall a. Int -> Parser a -> Parser (Array a)
replicate (I# total) (Parser (ParserLevity f)) =
  Parser (ParserLevity (\m s0 -> case newArray# total (die "replicate") s0 of
    (# s1, xs0 #) -> go 0# xs0 m s1
  ))
  where
  go :: Int# -> MutableArray# s a -> Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# s 'LiftedRep (Array a) #)
  go !n !xs !m0 !s0 = case n <# total of
    1# -> case f m0 s0 of
      (# s1, (# m1, v #) #) -> case v of
        (# (# #) | #) -> (# s1, (# m1, (# (# #) | #) #) #)
        (# | x #) -> case writeArray# xs n x s1 of
          s2 -> go (n +# 1# ) xs m1 s2
    _ -> case unsafeFreezeArray# xs s0 of
      (# s1, xsFrozen #) -> (# s1, (# m0, (# | Array xsFrozen #) #) #)
        
foldlIntersperseParserUntilEnd :: forall a b.
     Parser b -- ^ separator, result is discarded
  -> a -- ^ initial accumulator
  -> (a -> Parser a) -- ^ parser that takes previous accumulator
  -> Parser a
foldlIntersperseParserUntilEnd sep a0 p = isEndOfInput >>= \case
  True -> return a0
  False -> do
    a1 <- p a0
    let go !a = isEndOfInput >>= \case
          True -> return a
          False -> do
            sep
            p a >>= go
    go a1

-- replicateIntersperseUntilEnd :: forall a b. Parser b -> Parser a -> Parser (Array a)
-- replicateIntersperseUntilEnd = _

-- replicateUntilMember :: forall a. ByteSet -> Parser a -> Parser (Array a)
-- replicateUntilMember !set (Parser (ParserLevity f)) = Parser (ParserLevity (go 0# [])) where

replicateUntilEnd :: forall a. Parser a -> Parser (Array a)
replicateUntilEnd (Parser (ParserLevity f)) = Parser (ParserLevity (go 0# [])) where
  go :: Int# -> [a] -> Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# s 'LiftedRep (Array a) #)
  go !n !xs !m !s0 = withNonEmpty m s0
    (\s -> 
      let theArray :: Array a
          !theArray = reverseArrayFromListN "replicateUntilEnd" (I# n) xs
       in (# s, (# (# (# #) | #), (# | theArray #) #) #)
    )
    (\_ theBytes stream s1 -> case f (# | (# theBytes, stream #) #) s1 of
      (# s2, (# leftovers, res #) #) -> case res of
        (# (# #) | #) -> (# s2, (# leftovers, (# (# #) | #) #) #)
        (# | !x #) -> go (n +# 1# ) (x : xs) leftovers s2
    )

replicateUntilMember :: forall a. ByteSet -> Parser a -> Parser (Array a)
replicateUntilMember !set (Parser (ParserLevity f)) = Parser (ParserLevity (go 0# [])) where
  go :: Int# -> [a] -> Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# s 'LiftedRep (Array a) #)
  go !n !xs !m !s0 = withNonEmpty m s0
    (\s1 -> (# s1, (# (# (# #) | #), (# (# #) | #) #) #))
    (\theByte theBytes stream s1 -> case ByteSet.member (W8# theByte) set of
      True -> 
        let theArray :: Array a
            !theArray = reverseArrayFromListN "replicateUntilMember" (I# n) xs
         in (# s1, (# (# | (# theBytes, stream #) #), (# | theArray #) #) #)
      False -> case f (# | (# theBytes, stream #) #) s1 of
        (# s2, (# leftovers, res #) #) -> case res of
          (# (# #) | #) -> (# s2, (# leftovers, (# (# #) | #) #) #)
          (# | !x #) -> go (n +# 1# ) (x : xs) leftovers s2
    )

die :: String -> a
die fun = error $ "Packed.Bytes.Parser." ++ fun
  ++ ": Implmentation error in this function. Please open a bug report."

createArray
  :: Int
  -> a
  -> (forall s. MutableArray s a -> ST s ())
  -> Array a
createArray n x f = runST $ do
  ma <- PM.newArray n x
  f ma
  PM.unsafeFreezeArray ma

reverseArrayFromListN :: String -> Int -> [a] -> Array a
reverseArrayFromListN funcName n l =
  createArray n (die funcName) $ \mi ->
    let go !i (x:xs) = do
          PM.writeArray mi i x
          go (i - 1) xs
        go !_ [] = return ()
     in go (n - 1) l

-- {-# INLINE replicateUntilTemplate #-}
-- replicateUntilTemplate :: forall a. (Word# -> Int#) -> Parser a -> Parser (Array a)
-- replicateUntilTemplate predicate (Parser (ParserLevity f)) = Parser (ParserLevity (go 0# [])) where
--   go :: Int# -> [a] -> Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# s LiftedRep (Array a) #)
--   go n xs m s0 = withNonEmpty m s0
--   -- go !xs (# (# #) | #) s0 = (# s0, (# (# (# #) | #), (# (# #) | #) #) #)
--   -- go !xs (# | leftovers@(# bytes0@(# arr, off, len #), !stream0@(ByteStream streamFunc) #) #) s0 = 
--   --   if I# len > 0
--   --     then if predicate (PM.indexByteArray (ByteArray arr) (I# off))
--   --       then (# s0, (# (# | (# bytes0, stream0 #) #), (# (# #) | #) #) #)
--   --       else case f (# | (# unsafeDrop# 1 bytes0, stream0, stream0 #) #) s0 
--   --     else case nextNonEmpty stream0 s0 of
--   --       (# s1, r #) -> case r of
--   --         (# (# #) | #) -> (# s1, (# (# (# #) | #), (# (# #) | #) #) #)
--   --         (# | (# bytes1@(# arr1, off1, _ #), stream1 #) #) -> if predicate (PM.indexByteArray (ByteArray arr1) (I# off1))
--   --           then (# s1, (# (# | (# unsafeDrop# 1 bytes1, stream1 #) #), (# | () #) #) #)
--   --           else (# s1, (# (# | (# bytes1, stream1 #) #), (# (# #) | #) #) #)

appendMaybeBytes :: Maybe# Bytes# -> Bytes# -> Bytes#
appendMaybeBytes (# (# #) | #) theBytes = theBytes
appendMaybeBytes (# | b #) theBytes = unboxBytes (B.append (boxBytes b) (boxBytes theBytes))

maybeBytesToBytes :: Maybe# Bytes# -> Bytes
maybeBytesToBytes (# (# #) | #) = B.empty
maybeBytesToBytes (# | theBytes #) = boxBytes theBytes

maybeBytesToBytes# :: Maybe# Bytes# -> Bytes#
maybeBytesToBytes# (# (# #) | #) = case runRW# makeEmptyByteArray# of
  (# _, emptyArr #) -> (# emptyArr, 0#, 0# #)
maybeBytesToBytes# (# | theBytes #) = theBytes

makeEmptyByteArray# :: State# s -> (# State# s, ByteArray# #)
makeEmptyByteArray# s0 = case newByteArray# 0# s0 of
  (# s1, marr #) -> unsafeFreezeByteArray# marr s1

skipSpace :: Parser ()
skipSpace = Parser skipSpaceUnboxed

decimalWord :: Parser Word
decimalWord = Parser (boxWordParser decimalWordUnboxed)

mapParser :: (a -> b) -> Parser a -> Parser b
mapParser f p = bindLifted p (pureParser . f)

pureParser :: a -> Parser a
pureParser a = Parser $ ParserLevity $ \leftovers0 s0 ->
  (# s0, (# leftovers0, (# | a #) #) #)

bindLifted :: Parser a -> (a -> Parser b) -> Parser b
bindLifted (Parser (ParserLevity f)) g = Parser $ ParserLevity $ \leftovers0 s0 -> case f leftovers0 s0 of
  (# s1, (# leftovers1, val #) #) -> case val of
    (# (# #) | #) -> (# s1, (# leftovers1, (# (# #) | #) #) #)
    (# | x #) -> case g x of
      Parser (ParserLevity k) -> k leftovers1 s1


bindWord :: ParserLevity 'WordRep Word# -> (Word# -> ParserLevity 'WordRep Word#) -> ParserLevity 'WordRep Word#
bindWord (ParserLevity f) g = ParserLevity $ \leftovers0 s0 -> case f leftovers0 s0 of
  (# s1, (# leftovers1, val #) #) -> case val of
    (# (# #) | #) -> (# s1, (# leftovers1, (# (# #) | #) #) #)
    (# | x #) -> case g x of
      ParserLevity k -> k leftovers1 s1

boxWord8Parser :: ParserLevity 'WordRep Word# -> ParserLevity 'LiftedRep Word8
boxWord8Parser p = ParserLevity $ \leftovers0 s0 ->
  case getParserLevity p leftovers0 s0 of
    (# s1, (# leftovers1, val #) #) -> case val of
      (# (# #) | #) -> (# s1, (# leftovers1, (# (# #) | #) #) #)
      (# | x #) -> (# s1, (# leftovers1, (# | W8# x #) #) #)

boxWordParser :: ParserLevity 'WordRep Word# -> ParserLevity 'LiftedRep Word
boxWordParser p = ParserLevity $ \leftovers0 s0 ->
  case getParserLevity p leftovers0 s0 of
    (# s1, (# leftovers1, val #) #) -> case val of
      (# (# #) | #) -> (# s1, (# leftovers1, (# (# #) | #) #) #)
      (# | x #) -> (# s1, (# leftovers1, (# | W# x #) #) #)

boxBytesWord8Parser ::
     ParserLevity ('TupleRep '[BytesRep, 'WordRep]) (# Bytes#, Word# #)
  -> ParserLevity 'LiftedRep (Bytes,Word8)
boxBytesWord8Parser p = ParserLevity $ \leftovers0 s0 ->
  case getParserLevity p leftovers0 s0 of
    (# s1, (# leftovers1, val #) #) -> case val of
      (# (# #) | #) -> (# s1, (# leftovers1, (# (# #) | #) #) #)
      (# | (# theBytes, theWord #) #) -> (# s1, (# leftovers1, (# | (boxBytes theBytes, W8# theWord) #) #) #)

boxBytesParser ::
     ParserLevity BytesRep Bytes#
  -> ParserLevity 'LiftedRep Bytes
boxBytesParser p = ParserLevity $ \leftovers0 s0 ->
  case getParserLevity p leftovers0 s0 of
    (# s1, (# leftovers1, val #) #) -> case val of
      (# (# #) | #) -> (# s1, (# leftovers1, (# (# #) | #) #) #)
      (# | theBytes #) -> (# s1, (# leftovers1, (# | boxBytes theBytes #) #) #)

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

unboxBytes :: Bytes -> Bytes#
unboxBytes (Bytes (ByteArray a) (I# b) (I# c)) = (# a,b,c #)

unboxInt :: Int -> Int#
unboxInt (I# i) = i

-- | Match either a single newline character @\'\\n\'@, or a carriage
-- return followed by a newline character @\"\\r\\n\"@.
endOfLine :: Parser ()
-- In the event of a failure in the carriage-return case, this
-- currently consumes a character from the leftovers that
-- should be left alone. Could be hand-written to be faster and
-- more correct.
endOfLine = do
  w <- any
  case w of
    10 -> return ()
    13 -> byte 10
    _ -> failure

failure :: Parser a
failure = Parser (ParserLevity (\m s -> (# s, (# m, (# (# #) | #) #) #)))
