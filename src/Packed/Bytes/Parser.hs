{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
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
  , StatefulParser(..)
  , Result(..)
  , PureResult(..)
  , Leftovers(..)
  , parseBytes
  , parseStreamST
  , parseStreamIO
  , decimalInt
  , decimalWord
  , decimalWordStarting
  , decimalDigitWord
  , optionalPlusMinus
  , optionalDecimalDigitWord
  , takeBytesWhileMember
  , takeBytesUntilMemberConsume
  , takeBytesUntilByteConsume
  , takeBytesUntilByte
  , skipUntilByteConsume
  , skipUntilByte
  , skipWhileByte
  , skipDigits
  , bytes
  , byte
  , byteTwo
  , byteThree
  , byteFour
  , optionalByte
  , peek
  , any
  , endOfInput
  , isEndOfInput
  , replicate
  , replicateUntilEnd
  -- , replicateIntersperseUntilEnd
  -- , replicateUntilByte
  , replicateIntersperseByte
  , replicateIntersperseMember
  , replicateIntersperseBytePrim
  , foldIntersperseParserUntilEnd
  , foldIntersperseByte
  , trie
  , triePure
  , trieReader
  , trieReaderState
  , trieReaderState_
  , failure
    -- * Context
  , scoped
    -- * Stateful
  , statefully
  , consumption
  , mutation
    -- * ASCII
  , skipSpace
  , endOfLine
  , takeBytesUntilEndOfLineConsume
  ) where

import Control.Applicative
import Control.Monad.ST (runST)
import Data.Char (ord)
import Data.Primitive (Prim,Array(..),MutableArray(..),PrimArray(..))
import Data.Semigroup (Semigroup)
import GHC.Int (Int(I#))
import GHC.ST (ST(..))
import GHC.Types (TYPE,RuntimeRep(..),IO(..))
import GHC.Word (Word(W#),Word8(W8#))
import Packed.Bytes (Bytes(..))
import Packed.Bytes.Set (ByteSet)
import Packed.Bytes.Small (ByteArray(..))
import Packed.Bytes.Stream.ST (ByteStream(..))
import Packed.Bytes.Trie (Trie)
import Prelude hiding (any,replicate)

import qualified Control.Monad
import qualified Data.Primitive as PM
import qualified Data.Semigroup as SG
import qualified Packed.Bytes as B
import qualified Packed.Bytes.Set as ByteSet
import qualified Packed.Bytes.Small as BA
import qualified Packed.Bytes.Stream.ST as Stream
import qualified Packed.Bytes.Trie as Trie
import qualified Packed.Bytes.Window as BAW

import GHC.Exts (State#,Int#,ByteArray#,Word#,(+#),(-#),(>#),
  (<#),(==#),(>=#),(*#),
  MutableArray#,MutableByteArray#,writeArray#,unsafeFreezeArray#,newArray#,
  unsafeFreezeByteArray#,newByteArray#,runRW#,
  plusWord#,timesWord#,indexWord8Array#,eqWord#,fromListN,andI#,
  clz8#, or#, neWord#, uncheckedShiftL#,int2Word#,word2Int#,quotInt#,
  shrinkMutableByteArray#,copyMutableByteArray#,
  RealWorld)

type Bytes# = (# ByteArray#, Int#, Int# #)
type Maybe# (a :: TYPE r) = (# (# #) | a #)
type Either# a (b :: TYPE r) = (# a | b #)
type Leftovers# s = (# Bytes# , ByteStream s #)
type Result# e c s (r :: RuntimeRep) (a :: TYPE r) =
  (# Maybe# (Leftovers# s), Either# (Maybe e) a, c #)
type BytesRep = 'TupleRep '[ 'UnliftedRep, 'IntRep, 'IntRep ]

debugMode :: Bool
debugMode = False

data Result e c s a = Result
  { resultLeftovers :: !(Maybe (Leftovers s))
  , resultValue :: !(Either (Maybe e) a)
  , resultContext :: c
  }

data Leftovers s = Leftovers
  { leftoversChunk :: {-# UNPACK #-} !Bytes
    -- ^ The last chunk pulled from the stream
  , leftoversStream :: ByteStream s
    -- ^ The remaining stream
  }

data PureResult e c a = PureResult
  { pureResultLeftovers :: {-# UNPACK #-} !Bytes
  , pureResultValue :: !(Either (Maybe e) a)
  , pureResultContext :: c
  } deriving (Show,Eq)

parseBytes :: Bytes -> c -> Parser e c a -> PureResult e c a
parseBytes theBytes c0 p = runST $ do
  Result mleftovers mval c1 <- parseStreamST (Stream.fromBytes theBytes) c0 p
  theLeftovers <- case mleftovers of
    Nothing -> return B.empty
    Just (Leftovers chunk stream) -> do
      others <- Stream.toBytes stream
      return (B.append chunk others)
  return (PureResult theLeftovers mval c1)

parseStreamST :: ByteStream s -> c -> Parser e c a -> ST s (Result e c s a)
parseStreamST stream c0 (Parser (ParserLevity f)) = ST $ \s0 ->
  case f c0 (# | (# (# unboxByteArray BA.empty, 0#, 0# #), stream #) #) s0 of
    (# s1, r #) -> (# s1, boxResult r #)

parseStreamIO :: ByteStream RealWorld -> c -> Parser e c a -> IO (Result e c RealWorld a)
parseStreamIO stream c0 (Parser (ParserLevity f)) = IO $ \s0 ->
  case f c0 (# | (# (# unboxByteArray BA.empty, 0#, 0# #), stream #) #) s0 of
    (# s1, r #) -> (# s1, boxResult r #)

boxResult :: Result# e c s 'LiftedRep a -> Result e c s a
boxResult (# leftovers, val, c #) = case val of
  (# err | #) -> Result (boxLeftovers leftovers) (Left err) c
  (# | a #) -> Result (boxLeftovers leftovers) (Right a) c

boxLeftovers :: Maybe# (Leftovers# s) -> Maybe (Leftovers s)
boxLeftovers (# (# #) | #) = Nothing
boxLeftovers (# | (# theBytes, stream #) #) = Just (Leftovers (boxBytes theBytes) stream)

newtype Parser e c a = Parser (ParserLevity e c 'LiftedRep a)

instance Functor (Parser e c) where
  fmap = mapParser

-- Remember to write liftA2 by hand at some point.
instance Applicative (Parser e c) where
  pure = pureParser
  (<*>) = Control.Monad.ap
  (*>) = sequenceRight
  (<*) = sequenceLeft
  {-# INLINE (*>) #-}
  {-# INLINE (<*) #-}

instance Monad (Parser e c) where
  return = pure
  (>>=) = bindLifted

instance Semigroup a => Semigroup (Parser e c a) where
  (<>) = liftA2 (SG.<>)

instance Monoid a => Monoid (Parser e c a) where
  mempty = pure mempty

instance Functor (StatefulParser e c s) where
  fmap = mapStatefulParser

instance Applicative (StatefulParser e c s) where
  pure = pureStatefulParser
  (<*>) = Control.Monad.ap

instance Monad (StatefulParser e c s) where
  return = pure
  (>>=) = bindStateful

instance Semigroup a => Semigroup (StatefulParser e c s a) where
  (<>) = liftA2 (SG.<>)

instance Monoid a => Monoid (StatefulParser e c s a) where
  mempty = pure mempty


newtype ParserLevity e c (r :: RuntimeRep) (a :: TYPE r) = ParserLevity
  { getParserLevity :: forall s.
       c
    -> Maybe# (Leftovers# s)
    -> State# s
    -> (# State# s, Result# e c s r a #)
  }

-- | A parser that can interleave arbitrary 'ST' effects with parsing.
newtype StatefulParser e c s a = StatefulParser
  { getStatefulParser ::
       c
    -> Maybe# (Leftovers# s)
    -> State# s
    -> (# State# s, Result# e c s 'LiftedRep a #)
  }

sequenceRight :: Parser e c a -> Parser e c b -> Parser e c b
{-# NOINLINE[2] sequenceRight #-}
sequenceRight (Parser (ParserLevity f)) (Parser (ParserLevity g)) =
  Parser $ ParserLevity $ \c0 leftovers0 s0 -> case f c0 leftovers0 s0 of
    (# s1, (# leftovers1, val, c1 #) #) -> case val of
      (# r | #) -> (# s1, (# leftovers1, (# r | #), c1 #) #)
      (# | _ #) -> g c1 leftovers1 s1

sequenceLeft :: Parser e c a -> Parser e c b -> Parser e c a
{-# NOINLINE[2] sequenceLeft #-}
sequenceLeft (Parser (ParserLevity f)) (Parser (ParserLevity g)) =
  Parser $ ParserLevity $ \c0 leftovers0 s0 -> case f c0 leftovers0 s0 of
    (# s1, (# leftovers1, val, c1 #) #) -> case val of
      (# r | #) -> (# s1, (# leftovers1, (# r | #), c1 #) #)
      (# | a #) -> case g c1 leftovers1 s1 of
        (# s2, (# leftovers2, val2, c2 #) #) -> case val2 of
          (# r | #) -> (# s2, (# leftovers2, (# r | #), c2 #) #)
          (# | _ #) -> (# s2, (# leftovers2, (# | a #), c2 #) #)

bytesIndex :: Bytes# -> Int -> Word8
bytesIndex (# arr, off, _ #) ix = BA.unsafeIndex (ByteArray arr) (I# off + ix)

word8ToWord :: Word8 -> Word
word8ToWord = fromIntegral

wordToInt :: Word -> Int
wordToInt = fromIntegral

-- option :: a -> Parser e c a -> Parser e c a
-- option a (

nextNonEmpty :: ByteStream s -> State# s -> (# State# s, Maybe# (Leftovers# s) #)
nextNonEmpty (ByteStream f) s0 = case f s0 of
  (# s1, r #) -> case r of
    (# (# #) | #) -> (# s1, (# (# #) | #) #)
    (# | (# theBytes@(# _,_,len #), stream #) #) -> case len of
      0# -> nextNonEmpty stream s1
      _ -> (# s1, (# | (# theBytes, stream #) #) #)

{-# INLINE withNonEmpty #-}
withNonEmpty :: forall e c s (r :: RuntimeRep) (b :: TYPE r).
     Maybe# (Leftovers# s)
  -> State# s
  -> (State# s -> (# State# s, Result# e c s r b #))
  -> (Word# -> Bytes# -> ByteStream s -> State# s -> (# State# s, Result# e c s r b #))
     -- The first argument is a Word8, not a full machine word.
     -- The second argument is the complete,non-empty chunk
     -- with the head byte still intact.
  -> (# State# s, Result# e c s r b #)
withNonEmpty (# (# #) | #) s0 g _ = g s0
withNonEmpty (# | (# bytes0@(# arr0,off0,len0 #), stream0 #) #) s0 g f = case len0 ># 0# of
  1# -> f (indexWord8Array# arr0 off0) bytes0 stream0 s0
  _ -> case nextNonEmpty stream0 s0 of
    (# s1, r #) -> case r of
      (# (# #) | #) -> g s1
      (# | (# bytes1@(# arr1, off1, _ #), stream1 #) #) -> 
        f (indexWord8Array# arr1 off1) bytes1 stream1 s1

-- Internal function for functions that can take a special
-- faster case (involving fewer bounds checks and pattern
-- matches) if at least n bytes are present. The value n
-- should be at least 1.
withAtLeast :: forall e c s (r :: RuntimeRep) (b :: TYPE r).
     Maybe# (Leftovers# s)
  -> Int# -- number of bytes to require
  -> State# s
  -> (Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# e c s r b #))
     -- This action is taken if not enough bytes are present
     -- or if the stream is empty.
  -> (Bytes# -> ByteStream s -> State# s -> (# State# s, Result# e c s r b #))
     -- The first argument is the complete,non-empty chunk
     -- with at least n bytes present. This action is taken
     -- if at least the right number of bytes were present.
  -> (# State# s, Result# e c s r b #)
{-# INLINE withAtLeast #-}
withAtLeast (# (# #) | #) _ s0 g _ = g (# (# #) | #) s0
withAtLeast (# | (# bytes0@(# arr0,off0,len0 #), stream0 #) #) n s0 g f = case len0 >=# n of
  1# -> f bytes0 stream0 s0
  _ -> g (# | (# bytes0, stream0 #) #) s0

decimalDigit :: ParserLevity e c 'WordRep Word#
decimalDigit = ParserLevity $ \c0 leftovers0 s0 -> case leftovers0 of
  (# (# #) | #) -> (# s0, (# (# (# #) | #), (# Nothing | #), c0 #) #)
  (# | (# bytes0@(# _,_,len #), stream0 #) #) ->
    let !(# s1, r #) = case len of
          0# -> nextNonEmpty stream0 s0
          _ -> (# s0, (# | (# bytes0, stream0 #) #) #)
     in case r of
          (# (# #) | #) -> (# s1, (# (# (# #) | #), (# Nothing | #), c0 #) #)
          (# | (# bytes1, stream1 #) #) ->
            let !w = word8ToWord (bytesIndex bytes1 0) - 48
             in if w < 10
                  then (# s1, (# (# | (# unsafeDrop# 1# bytes1, stream1 #) #), (# | unboxWord w #), c0 #) #)
                  else (# s1, (# (# | (# bytes1, stream1 #) #), (# Nothing | #), c0 #) #)

optionalDecimalDigit :: ParserLevity e c ('SumRep '[ 'TupleRep '[], 'WordRep]) (Maybe# Word#)
optionalDecimalDigit = ParserLevity $ \c0 leftovers0 s0 -> case leftovers0 of
  (# (# #) | #) -> (# s0, (# (# (# #) | #), (# | (# (# #) | #) #), c0 #) #)
  (# | (# bytes0@(# _,_,len #), stream0 #) #) ->
    let !(# s1, r #) = case len of
          0# -> nextNonEmpty stream0 s0
          _ -> (# s0, (# | (# bytes0, stream0 #) #) #)
     in case r of
          (# (# #) | #) -> (# s1, (# (# (# #) | #), (# Nothing | #), c0 #) #)
          (# | (# bytes1, stream1 #) #) ->
            let !w = word8ToWord (bytesIndex bytes1 0) - 48
             in if w < 10
                  then (# s1, (# (# | (# unsafeDrop# 1# bytes1, stream1 #) #), (# | (# | unboxWord w #) #), c0 #) #)
                  else (# s1, (# (# | (# bytes1, stream1 #) #), (# | (# (# #) | #) #), c0 #) #)

decimalContinue :: Word# -> ParserLevity e c 'WordRep Word#
decimalContinue theWord = ParserLevity (action theWord) where
  action :: Word# -> c -> Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# e c s 'WordRep Word# #)
  action !w0 c0 (# (# #) | #) s0 = (# s0, (# (# (# #) | #), (# | w0 #), c0 #) #)
  action !w0 c0 (# | leftovers0 #) s0 = go w0 c0 leftovers0 s0
  go :: Word# -> c -> Leftovers# s -> State# s -> (# State# s, Result# e c s 'WordRep Word# #)
  go !w0 c0 leftovers0@(# (# _,_,len #), !stream0 #) s0 = 
    let !(# s1, leftovers1 #) = case len of
          0# -> nextNonEmpty stream0 s0
          _ -> (# s0, (# | leftovers0 #) #)
     in case leftovers1 of
          (# (# #) | #) -> (# s1, (# (# (# #) | #), (# | w0 #), c0 #) #)
          (# | (# bytes1, stream1 #) #) ->
            let !w = word8ToWord (bytesIndex bytes1 0) - 48
             in if w < 10
                  then go (plusWord# (timesWord# w0 10##) (unboxWord w)) c0 (# unsafeDrop# 1# bytes1, stream1 #) s1
                  else (# s1, (# (# | (# bytes1, stream1 #) #), (# | w0 #), c0 #) #)

decimalWordUnboxed :: ParserLevity e c 'WordRep Word#
decimalWordUnboxed = bindWord decimalDigit decimalContinue

skipSpaceUnboxed :: ParserLevity e c 'LiftedRep ()
skipSpaceUnboxed = ParserLevity go where
  go :: c -> Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# e c s 'LiftedRep () #)
  go c0 (# (# #) | #) s0 = (# s0, (# (# (# #) | #), (# | () #), c0 #) #)
  go c0 (# | (# bytes0@(# arr, off, len #), !stream0@(ByteStream streamFunc) #) #) s0 = case BAW.findNonAsciiSpace' (I# off) (I# len) (ByteArray arr) of
    (# (# #) | #) -> case streamFunc s0 of
      (# s1, r #) -> go c0 r s1
    (# | ix #) -> (# s0, (# (# | (# unsafeDrop# (ix -# off) bytes0, stream0 #) #), (# | () #), c0 #) #)

-- | Takes all 'Bytes' until the first occurrence of a byte in the
--   provided set. The bytes leading up to it are returned as the
--   first element of the tuple. The matching byte itself is consumed
--   from the input and returned as the second element of the tuple. This
--   parser will fail if the input ends before a byte from the set is
--   encountered.
{-# INLINE takeBytesUntilMemberConsume #-}
takeBytesUntilMemberConsume :: ByteSet -> Parser e c (Bytes,Word8)
takeBytesUntilMemberConsume (ByteSet.ByteSet (ByteArray set)) = Parser (boxBytesWord8Parser (takeBytesUntilMemberConsumeUnboxed set))

{-# NOINLINE takeBytesUntilMemberConsumeUnboxed #-}
takeBytesUntilMemberConsumeUnboxed :: ByteArray# -> ParserLevity e c ('TupleRep '[BytesRep, 'WordRep]) (# Bytes#, Word# #)
takeBytesUntilMemberConsumeUnboxed !set = ParserLevity (go (# (# #) | #)) where
  go :: Maybe# Bytes# -> c -> Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# e c s ('TupleRep '[BytesRep, 'WordRep]) (# Bytes#, Word# #) #)
  go !_ c0 (# (# #) | #) s0 = (# s0, (# (# (# #) | #), (# Nothing | #), c0 #) #)
  go !mbytes c0 (# | (# bytes0@(# arr, off, len #), !stream0@(ByteStream streamFunc) #) #) s0 = case BAW.findMemberByte (I# off) (I# len) (ByteSet.ByteSet (ByteArray set)) (ByteArray arr) of
    Nothing -> case streamFunc s0 of
      (# s1, r #) -> go (# | appendMaybeBytes mbytes bytes0 #) c0 r s1
    Just (I# ix, W8# w) -> (# s0, (# (# | (# unsafeDrop# ((ix -# off) +# 1# ) bytes0, stream0 #) #), (# | (# appendMaybeBytes mbytes (# arr, off, ix -# off #), w #) #), c0 #) #)

-- | Takes all 'Bytes' until the first occurrence of the given byte.
--   The bytes leading up to it are returned, and the matching byte
--   itself is consumed from the input.
{-# INLINE takeBytesUntilByteConsume #-}
takeBytesUntilByteConsume :: Word8 -> Parser e c Bytes
takeBytesUntilByteConsume (W8# theByte) = Parser (boxBytesParser (takeBytesUntilByteConsumeUnboxed theByte))

{-# NOINLINE takeBytesUntilByteConsumeUnboxed #-}
takeBytesUntilByteConsumeUnboxed :: Word# -> ParserLevity e c BytesRep Bytes#
takeBytesUntilByteConsumeUnboxed !theByte = ParserLevity (go (# (# #) | #)) where
  go :: Maybe# Bytes# -> c -> Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# e c s BytesRep Bytes# #)
  go !_ c0 (# (# #) | #) s0 = (# s0, (# (# (# #) | #), (# Nothing | #), c0 #) #)
  go !mbytes c0 (# | (# bytes0@(# arr, off, len #), !stream0@(ByteStream streamFunc) #) #) s0 = case BAW.findByte (I# off) (I# len) (W8# theByte) (ByteArray arr) of
    Nothing -> case streamFunc s0 of
      (# s1, r #) -> go (# | appendMaybeBytes mbytes bytes0 #) c0 r s1
    Just (I# ix) -> (# s0, (# (# | (# unsafeDrop# ((ix -# off) +# 1# ) bytes0, stream0 #) #), (# | appendMaybeBytes mbytes (# arr, off, ix -# off #) #), c0 #) #)

-- | Takes all 'Bytes' until the first occurrence of the given byte.
--   The bytes leading up to it are returned, and the matching byte
--   itself remains part of the input.
{-# INLINE takeBytesUntilByte #-}
takeBytesUntilByte :: Word8 -> Parser e c Bytes
takeBytesUntilByte (W8# theByte) = Parser (boxBytesParser (takeBytesUntilByteUnboxed theByte))

{-# NOINLINE takeBytesUntilByteUnboxed #-}
takeBytesUntilByteUnboxed :: Word# -> ParserLevity e c BytesRep Bytes#
-- fix this. It should succeed if it reaches the end of the input.
takeBytesUntilByteUnboxed !theByte = ParserLevity (go (# (# #) | #)) where
  go :: Maybe# Bytes# -> c -> Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# e c s BytesRep Bytes# #)
  go !_ c0 (# (# #) | #) s0 = (# s0, (# (# (# #) | #), (# Nothing | #), c0 #) #)
  go !mbytes c0 (# | (# bytes0@(# arr, off, len #), !stream0@(ByteStream streamFunc) #) #) s0 = case BAW.findByte (I# off) (I# len) (W8# theByte) (ByteArray arr) of
    Nothing -> case streamFunc s0 of
      (# s1, r #) -> go (# | appendMaybeBytes mbytes bytes0 #) c0 r s1
    Just (I# ix) -> (# s0, (# (# | (# unsafeDrop# (ix -# off) bytes0, stream0 #) #), (# | appendMaybeBytes mbytes (# arr, off, ix -# off #) #), c0 #) #)

skipDigits :: Parser e c ()
skipDigits = Parser (ParserLevity go) where
  go :: c -> Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# e c s 'LiftedRep () #)
  go c0 (# (# #) | #) s0 = (# s0, (# (# (# #) | #), (# | () #), c0 #) #)
  go c0 (# | (# bytes0@(# arr, off, len #), !stream0@(ByteStream streamFunc) #) #) s0 = case BAW.findNonDigit (I# off) (I# len) (ByteArray arr) of
    Nothing -> case streamFunc s0 of
      (# s1, r #) -> go c0 r s1
    Just (I# ix, _) -> (# s0, (# (# | (# unsafeDrop# (ix -# off) bytes0, stream0 #) #), (# | () #), c0 #) #)

-- | Skip all bytes until the given byte is encountered. Then, consume
-- the byte. This parser will fail if the end of the input is encountered.
skipUntilByteConsume :: Word8 -> Parser e c ()
skipUntilByteConsume (W8# w) = Parser (skipUntilByteConsumeUnboxed w)

skipUntilByteConsumeUnboxed :: Word# -> ParserLevity e c 'LiftedRep ()
skipUntilByteConsumeUnboxed !theByte = ParserLevity go where
  go :: c -> Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# e c s 'LiftedRep () #)
  go c0 (# (# #) | #) s0 = (# s0, (# (# (# #) | #), (# Nothing | #), c0 #) #)
  go c0 (# | (# bytes0@(# arr, off, len #), !stream0@(ByteStream streamFunc) #) #) s0 = case BAW.findByte (I# off) (I# len) (W8# theByte) (ByteArray arr) of
    Nothing -> case streamFunc s0 of
      (# s1, r #) -> go c0 r s1
    Just (I# ix) -> (# s0, (# (# | (# unsafeDrop# ((ix -# off) +# 1# ) bytes0, stream0 #) #), (# | () #), c0 #) #)

-- | Skips all bytes matching the given byte.
skipWhileByte :: Word8 -> Parser e c ()
-- TODO: Improve the performance of this. I suspect that we can travel a full machine word at a time.
-- We will need to add something to Packed.Bytes.Window to make this work.
skipWhileByte theByte = go where
  go = isEndOfInput >>= \case
    True -> return ()
    False -> do
      b <- peek
      if b == theByte
        then any >> go
        else return ()

skipUntilByte :: Word8 -> Parser e c ()
skipUntilByte (W8# theByte) = Parser (ParserLevity go) where
  go :: c -> Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# e c s 'LiftedRep () #)
  go c0 (# (# #) | #) s0 = (# s0, (# (# (# #) | #), (# | () #), c0 #) #)
  go c0 (# | (# bytes0@(# arr, off, len #), !stream0@(ByteStream streamFunc) #) #) s0 = case BAW.findByte (I# off) (I# len) (W8# theByte) (ByteArray arr) of
    Nothing -> case streamFunc s0 of
      (# s1, r #) -> go c0 r s1
    Just (I# ix) -> (# s0, (# (# | (# unsafeDrop# (ix -# off) bytes0, stream0 #) #), (# | () #), c0 #) #)

{-# INLINE takeBytesUntilEndOfLineConsume #-}
takeBytesUntilEndOfLineConsume :: Parser e c Bytes
takeBytesUntilEndOfLineConsume = Parser (boxBytesParser takeBytesUntilEndOfLineConsumeUnboxed)

{-# NOINLINE takeBytesUntilEndOfLineConsumeUnboxed #-}
takeBytesUntilEndOfLineConsumeUnboxed :: ParserLevity e c BytesRep Bytes#
takeBytesUntilEndOfLineConsumeUnboxed = ParserLevity (go (# (# #) | #)) where
  go :: Maybe# Bytes# -> c -> Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# e c s BytesRep Bytes# #)
  go !_ c0 (# (# #) | #) s0 = (# s0, (# (# (# #) | #), (# Nothing | #), c0 #) #)
  go !mbytes c0 (# | (# bytes0@(# arr0, off0, len0 #), !stream0@(ByteStream streamFunc) #) #) s0 = case BAW.findAnyByte2 (I# off0) (I# len0) 10 13 (ByteArray arr0) of
    Nothing -> case streamFunc s0 of
      (# s1, r #) -> go (# | appendMaybeBytes mbytes bytes0 #) c0 r s1
    Just (I# ix, W8# theByte) -> case theByte of
      10## -> (# s0, (# (# | (# unsafeDrop# ((ix -# off0) +# 1# ) bytes0, stream0 #) #), (# | appendMaybeBytes mbytes (# arr0, off0, ix -# off0 #) #), c0 #) #)
      -- second case means it was 13
      _ -> case ix <# (off0 +# len0 -# 1#) of
        1# -> case indexWord8Array# arr0 (ix +# 1# ) of
          10## -> (# s0, (# (# | (# unsafeDrop# ((ix -# off0) +# 2# ) bytes0, stream0 #) #), (# | appendMaybeBytes mbytes (# arr0, off0, ix -# off0 #) #), c0 #) #)
          _ -> (# s0, (# (# | (# unsafeDrop# (ix -# off0) bytes0, stream0 #) #), (# Nothing | #), c0 #) #)
        _ -> case nextNonEmpty stream0 s0 of
          (# s1, m #) -> case m of
            (# (# #) | #) -> (# s1, (# (# | (# unboxBytes (B.singleton 13), Stream.empty #) #), (# Nothing | #), c0 #) #)
            (# | (# bytes1@(# arr1, _, _ #), stream1 #) #) -> case indexWord8Array# arr1 0# of
              10## -> (# s1, (# (# | (# unsafeDrop# 1# bytes1, stream1 #) #), (# | appendMaybeBytes mbytes (# arr0, off0, ix -# off0 #) #), c0 #) #)
              _ -> (# s1, (# (# | (# unboxBytes (B.cons 13 (boxBytes bytes1)), stream1 #) #), (# Nothing | #), c0 #) #)

-- | Takes all 'Bytes' until the first occurrence of a byte in the
--   provided set. The bytes leading up to it are returned. The matching
--   byte is not consumed. This parser succeeds if the input ends before
--   a byte from the set is encountered.
takeBytesWhileMember :: ByteSet -> Parser e c Bytes
takeBytesWhileMember b = Parser (takeBytesWhileMemberUnboxed b)

takeBytesWhileMemberUnboxed :: ByteSet -> ParserLevity e c 'LiftedRep Bytes
takeBytesWhileMemberUnboxed !set = ParserLevity (go (# (# #) | #)) where
  go :: Maybe# Bytes# -> c -> Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# e c s 'LiftedRep Bytes #)
  go mbytes c0 (# (# #) | #) s0 = (# s0, (# (# (# #) | #), (# | maybeBytesToBytes mbytes #), c0 #) #)
  go mbytes c0 (# | (# bytes0@(# arr, off, len #), !stream0@(ByteStream streamFunc) #) #) s0 = case BAW.findNonMemberByte (I# off) (I# len) set (ByteArray arr) of
    Nothing -> case streamFunc s0 of
      (# s1, r #) -> go (# | appendMaybeBytes mbytes bytes0 #) c0 r s1
    Just (I# ix,!_) -> (# s0, (# (# | (# unsafeDrop# (ix -# off) bytes0, stream0 #) #), (# | boxBytes (appendMaybeBytes mbytes (# arr, off, ix -# off #)) #), c0 #) #)

-- Argument should be an 8-bit word.
byteUnboxed :: Word# -> ParserLevity e c 'LiftedRep ()
byteUnboxed expected = ParserLevity go where
  go :: c -> Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# e c s 'LiftedRep () #)
  go c0 m s0 = withNonEmpty m s0
    (\s -> (# s, (# (# (# #) | #), (# Nothing | #), c0 #) #))
    (\actual theBytes stream s -> case eqWord# expected actual of
      1# -> (# s, (# (# | (# unsafeDrop# 1# theBytes, stream #) #), (# | () #), c0 #) #)
      _ -> (# s, (# (# | (# theBytes, stream #) #), (# Nothing | #), c0 #) #)
    )

-- Arguments should be 8-bit words. This should be exactly
-- equivalent to calling byteUnboxed twice.
byteTwoUnboxed :: Word# -> Word# -> ParserLevity e c 'LiftedRep ()
byteTwoUnboxed expected0 expected1 = ParserLevity go where
  go :: c -> Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# e c s 'LiftedRep () #)
  go c0 m s0 = withAtLeast m 2# s0
    (getParserLevity
      (bindBoxed (byteUnboxed expected0)
        (\_ -> byteUnboxed expected1)
      ) c0
    )
    (\theBytes@(# arr, off, _ #) stream s ->
        let ax = eqWord# expected0 (indexWord8Array# arr off)
            bx = eqWord# expected1 (indexWord8Array# arr (off +# 1#))
         in case ax `andI#` bx of
          1# -> (# s, (# (# | (# unsafeDrop# 2# theBytes, stream #) #), (# | () #), c0 #) #)
          _ ->
            let ay = int2Word# (neWord# expected0 (indexWord8Array# arr off))
                by = int2Word# (neWord# expected1 (indexWord8Array# arr (off +# 1#)))
                artifact = uncheckedShiftL# ay 1# `or#` by
                res = 2# -# (8# -# word2Int# (clz8# artifact))
             in (# s, (# (# | (# unsafeDrop# res theBytes, stream #) #), (# Nothing | #), c0 #) #)
    )

-- Arguments should be 8-bit words. This should be exactly
-- equivalent to calling byteUnboxed three times.
byteThreeUnboxed :: Word# -> Word# -> Word# -> ParserLevity e c 'LiftedRep ()
byteThreeUnboxed expected0 expected1 expected2 = ParserLevity go where
  go :: c -> Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# e c s 'LiftedRep () #)
  go c0 m s0 = withAtLeast m 3# s0
    (getParserLevity
      (bindBoxed (byteUnboxed expected0)
        (\_ -> bindBoxed (byteUnboxed expected1)
          (\_ -> byteUnboxed expected2)
        )
      ) c0
    )
    (\theBytes@(# arr, off, _ #) stream s ->
        let ax = eqWord# expected0 (indexWord8Array# arr off)
            bx = eqWord# expected1 (indexWord8Array# arr (off +# 1#))
            cx = eqWord# expected2 (indexWord8Array# arr (off +# 2#))
         in case ax `andI#` bx `andI#` cx of
          1# -> (# s, (# (# | (# unsafeDrop# 3# theBytes, stream #) #), (# | () #), c0 #) #)
          _ ->
            let ay = int2Word# (neWord# expected0 (indexWord8Array# arr off))
                by = int2Word# (neWord# expected1 (indexWord8Array# arr (off +# 1#)))
                cy = int2Word# (neWord# expected2 (indexWord8Array# arr (off +# 2#)))
                artifact = uncheckedShiftL# ay 2#
                     `or#` uncheckedShiftL# by 1#
                     `or#` cy
                res = 3# -# (8# -# word2Int# (clz8# artifact))
             in (# s, (# (# | (# unsafeDrop# res theBytes, stream #) #), (# Nothing | #), c0 #) #)
    )

-- Arguments should be 8-bit words. This should be exactly
-- equivalent to calling byteUnboxed four times.
byteFourUnboxed :: Word# -> Word# -> Word# -> Word# -> ParserLevity e c 'LiftedRep ()
byteFourUnboxed expected0 expected1 expected2 expected3 = ParserLevity go where
  go :: c -> Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# e c s 'LiftedRep () #)
  go c0 m s0 = withAtLeast m 4# s0
    (getParserLevity
      (bindBoxed (byteUnboxed expected0)
        (\_ -> bindBoxed (byteUnboxed expected1)
          (\_ -> bindBoxed (byteUnboxed expected2)
            (\_ -> byteUnboxed expected3)
          )
        )
      ) c0
    )
    (\theBytes@(# arr, off, _ #) stream s ->
        let ax = eqWord# expected0 (indexWord8Array# arr off)
            bx = eqWord# expected1 (indexWord8Array# arr (off +# 1#))
            cx = eqWord# expected2 (indexWord8Array# arr (off +# 2#))
            dx = eqWord# expected3 (indexWord8Array# arr (off +# 3#))
         in case ax `andI#` bx `andI#` cx `andI#` dx of
          1# -> (# s, (# (# | (# unsafeDrop# 4# theBytes, stream #) #), (# | () #), c0 #) #)
          _ ->
            let ay = int2Word# (neWord# expected0 (indexWord8Array# arr off))
                by = int2Word# (neWord# expected1 (indexWord8Array# arr (off +# 1#)))
                cy = int2Word# (neWord# expected2 (indexWord8Array# arr (off +# 2#)))
                dy = int2Word# (neWord# expected3 (indexWord8Array# arr (off +# 3#)))
                artifact = uncheckedShiftL# ay 3#
                     `or#` uncheckedShiftL# by 2#
                     `or#` uncheckedShiftL# cy 1#
                     `or#` dy
                res = 4# -# (8# -# word2Int# (clz8# artifact))
             in (# s, (# (# | (# unsafeDrop# res theBytes, stream #) #), (# Nothing | #), c0 #) #)
    )

optionalByteUnboxed :: Word8 -> ParserLevity e c 'LiftedRep Bool
optionalByteUnboxed (W8# expected) = ParserLevity go where
  go :: c -> Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# e c s 'LiftedRep Bool #)
  go c0 m s0 = withNonEmpty m s0
    (\s -> (# s, (# (# (# #) | #), (# | False #), c0 #) #))
    (\actual theBytes stream s -> case eqWord# expected actual of
      1# -> (# s, (# (# | (# unsafeDrop# 1# theBytes, stream #) #), (# | True #), c0 #) #)
      _ -> (# s, (# (# | (# theBytes, stream #) #), (# | False #), c0 #) #)
    )

optionalPlusMinusUnboxed :: ParserLevity e c 'LiftedRep Bool
optionalPlusMinusUnboxed = ParserLevity go where
  go :: c -> Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# e c s 'LiftedRep Bool #)
  go c0 m s0 = withNonEmpty m s0
    (\s -> (# s, (# (# (# #) | #), (# | True #), c0 #) #))
    (\actual theBytes stream s -> case actual of
      43## -> (# s, (# (# | (# unsafeDrop# 1# theBytes, stream #) #), (# | True #), c0 #) #)
      45## -> (# s, (# (# | (# unsafeDrop# 1# theBytes, stream #) #), (# | False #), c0 #) #)
      _ -> (# s, (# (# | (# theBytes, stream #) #), (# | True #), c0 #) #)
    )

anyUnboxed :: ParserLevity e c 'WordRep Word#
anyUnboxed = ParserLevity go where
  go :: c -> Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# e c s 'WordRep Word# #)
  go c0 m s0 = withNonEmpty m s0
    (\s -> (# s, (# (# (# #) | #), (# Nothing | #), c0 #) #))
    (\theByte theBytes stream s ->
      (# s, (# (# | (# unsafeDrop# 1# theBytes, stream #) #), (# | theByte #), c0 #) #)
    )

peekUnboxed :: ParserLevity e c 'WordRep Word#
peekUnboxed = ParserLevity go where
  go :: c -> Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# e c s 'WordRep Word# #)
  go c0 m s0 = withNonEmpty m s0
    (\s -> (# s, (# (# (# #) | #), (# Nothing | #), c0 #) #))
    (\theByte theBytes stream s ->
      (# s, (# (# | (# theBytes, stream #) #), (# | theByte #), c0 #) #)
    )

-- | Returns true if there is no more input and false otherwise.
-- This parser always succeeds.
isEndOfInput :: Parser e c Bool
isEndOfInput = Parser (ParserLevity go) where
  go :: c -> Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# e c s 'LiftedRep Bool #)
  go c0 m s0 = withNonEmpty m s0
    (\s -> (# s, (# (# (# #) | #), (# | True #), c0 #) #))
    (\_ theBytes stream s -> 
      (# s, (# (# | (# theBytes, stream #) #), (# | False #), c0 #) #)
    )

endOfInputUnboxed :: ParserLevity e c 'LiftedRep ()
endOfInputUnboxed = ParserLevity go where
  go :: c -> Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# e c s 'LiftedRep () #)
  go c0 m s0 = withNonEmpty m s0
    (\s -> (# s, (# (# (# #) | #), (# | () #), c0 #) #))
    (\_ theBytes stream s -> 
      (# s, (# (# | (# theBytes, stream #) #), (# Nothing | #), c0 #) #)
    )

-- | Only succeed if there is no more input remaining.
endOfInput :: Parser e c ()
endOfInput = Parser endOfInputUnboxed

-- | Consume the next byte from the input.
any :: Parser e c Word8
any = Parser (boxWord8Parser anyUnboxed)

-- | Look at the next byte without consuming it. This parser will
-- fail if there is no more input.
peek :: Parser e c Word8
peek = Parser (boxWord8Parser peekUnboxed)

-- | Consume a byte matching the specified one.
byte :: Word8 -> Parser e c ()
{-# NOINLINE[2] byte #-}
byte (W8# theByte) = Parser (byteUnboxed theByte)

{-# RULES "byte{right,1 + 1 = 2}" [~2] forall a b. sequenceRight (byte a) (byte b) = byteTwo a b #-}
{-# RULES "byte{right,2 + 1 = 3}" [~2] forall a b c. sequenceRight (byteTwo a b) (byte c) = byteThree a b c #-}
{-# RULES "byte{right,1 + 2 = 3}" [~2] forall a b c. sequenceRight (byte a) (byteTwo b c) = byteThree a b c #-}
{-# RULES "byte{right,1 + 3 = 4}" [~2] forall a b c d. sequenceRight (byte a) (byteThree b c d) = byteFour a b c d #-}
{-# RULES "byte{right,2 + 2 = 4}" [~2] forall a b c d. sequenceRight (byteTwo a b) (byteTwo c d) = byteFour a b c d #-}
{-# RULES "byte{right,3 + 1 = 4}" [~2] forall a b c d. sequenceRight (byteThree a b c) (byte d) = byteFour a b c d #-}

{-# RULES "byte{left,1 + 1 = 2}" [~2] forall a b. sequenceLeft (byte a) (byte b) = byteTwo a b #-}
{-# RULES "byte{left,2 + 1 = 3}" [~2] forall a b c. sequenceLeft (byteTwo a b) (byte c) = byteThree a b c #-}
{-# RULES "byte{left,1 + 2 = 3}" [~2] forall a b c. sequenceLeft (byte a) (byteTwo b c) = byteThree a b c #-}

byteTwo :: Word8 -> Word8 -> Parser e c ()
{-# NOINLINE[2] byteTwo #-}
byteTwo (W8# a) (W8# b) = Parser (byteTwoUnboxed a b)

byteThree :: Word8 -> Word8 -> Word8 -> Parser e c ()
{-# NOINLINE[2] byteThree #-}
byteThree (W8# a) (W8# b) (W8# c) = Parser (byteThreeUnboxed a b c)

byteFour :: Word8 -> Word8 -> Word8 -> Word8 -> Parser e c ()
{-# NOINLINE[2] byteFour #-}
byteFour (W8# a) (W8# b) (W8# c) (W8# d) = Parser (byteFourUnboxed a b c d)

-- | Consume a byte matching the specified one. If the next byte
-- in the input does not match the given byte, it is not consumed.
-- This parser always succeeds even if there is no input. It returns
-- True if the byte was present (and consumed) and False is the byte
-- was not present.
optionalByte :: Word8 -> Parser e c Bool
optionalByte theByte = Parser (optionalByteUnboxed theByte)

-- | Consume a byte that is either a plus sign or a minus sign.
-- If neither of those two bytes follow or if the stream is empty,
-- consume nothing. This always succeeds. If a plus sign was consumed
-- or if nothing was consumed, returns True. If a minus sign was
-- consumed, returns False.
optionalPlusMinus :: Parser e c Bool
optionalPlusMinus = Parser optionalPlusMinusUnboxed

bytes :: Bytes -> Parser e c ()
bytes b = Parser (bytesUnboxed b)

bytesUnboxed :: Bytes -> ParserLevity e c 'LiftedRep ()
bytesUnboxed (Bytes parr poff plen) = ParserLevity (go (unboxInt poff)) where
  !(I# pend) = poff + plen
  go :: Int# -> c -> Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# e c s 'LiftedRep () #)
  go !ix c0 (# (# #) | #) s0 = case ix ==# pend of
    1# -> (# s0, (# (# (# #) | #), (# | () #), c0 #) #)
    _ -> (# s0, (# (# (# #) | #), (# Nothing | #), c0 #) #)
  go !ix c0 (# | leftovers@(# bytes0@(# arr, off, len #), !stream0@(ByteStream streamFunc) #) #) s0 = 
    case BAW.stripPrefixResumable (I# ix) (I# off) (I# (pend -# ix)) (I# len) parr (ByteArray arr) of
      (# (# #) | | #) -> (# s0, (# (# | leftovers #), (# Nothing | #), c0 #) #)
      (# | (# #) | #) -> (# s0, (# (# | (# unsafeDrop# (pend -# ix) bytes0, stream0 #) #), (# | () #), c0 #) #)
      (# | | (# #) #) -> case streamFunc s0 of
        (# s1, r #) -> go (ix +# len) c0 r s1

-- replicateUntilMember :: forall e c a. ByteSet -> Parser e c a -> Parser (Array a)
-- replicateUntilMember separators b p = go []
--   where
--   go :: [a] -> Parser
--   go !xs = 
  
-- | Repeat the parser a specified number of times. The implementation
--   is tail recursive and avoids building up a list as an intermediate
--   data structure. Instead, it writes to an array directly. 
replicate :: forall e c a. Int -> Parser e c a -> Parser e c (Array a)
replicate (I# total) (Parser (ParserLevity f)) =
  Parser (ParserLevity (\c0 m s0 -> case newArray# total (die "replicate") s0 of
    (# s1, xs0 #) -> go 0# xs0 c0 m s1
  ))
  where
  go :: Int# -> MutableArray# s a -> c -> Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# e c s 'LiftedRep (Array a) #)
  go !n !xs c0 !m0 !s0 = case n <# total of
    1# -> case f c0 m0 s0 of
      (# s1, (# m1, v, c1 #) #) -> case v of
        (# err | #) -> (# s1, (# m1, (# err | #), c1 #) #)
        (# | x #) -> case writeArray# xs n x s1 of
          s2 -> go (n +# 1# ) xs c1 m1 s2
    _ -> case unsafeFreezeArray# xs s0 of
      (# s1, xsFrozen #) -> (# s1, (# m0, (# | Array xsFrozen #), c0 #) #)
        
-- | Repeatly run the folding parser followed by the separator parser
-- until the end of the input is reached.
foldIntersperseParserUntilEnd :: 
     Parser e c b -- ^ separator, result is discarded
  -> a -- ^ initial accumulator
  -> (a -> Parser e c a) -- ^ parser that takes previous accumulator
  -> Parser e c a
foldIntersperseParserUntilEnd sep !a0 p = isEndOfInput >>= \case
  True -> return a0
  False -> do
    a1 <- p a0
    let go !a = isEndOfInput >>= \case
          True -> return a
          False -> do
            _ <- sep
            p a >>= go
    go a1

-- | Repeatly run the folding parser followed by consuming the separator.
-- Completes successfully when a byte other than the separator is encountered
-- or when the end of the input is reached.
foldIntersperseByte :: 
     Word8 -- ^ separator
  -> a -- ^ initial accumulator
  -> (a -> Parser e c a) -- ^ parser that takes previous accumulator
  -> Parser e c a
foldIntersperseByte !sep !a0 p = isEndOfInput >>= \case
  True -> return a0
  False -> do
    a1 <- p a0
    let go !a = isEndOfInput >>= \case
          True -> return a
          False -> do
            b <- peek
            if b == sep
              then do
                _ <- any
                p a >>= go
              else return a
    go a1

-- replicateIntersperseUntilEnd :: forall e c a b. Parser e c b -> Parser e c a -> Parser e c (Array a)
-- replicateIntersperseUntilEnd = _

-- replicateUntilMember :: forall e c a. ByteSet -> Parser e c a -> Parser (Array a)
-- replicateUntilMember !set (Parser (ParserLevity f)) = Parser (ParserLevity (go 0# [])) where


-- | Run the parser repeatedly until the end of the stream is
-- encountered.
replicateUntilEnd :: forall e c a. Parser e c a -> Parser e c (Array a)
replicateUntilEnd (Parser (ParserLevity f)) = Parser (ParserLevity (go 0# [])) where
  go :: Int# -> [a] -> c -> Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# e c s 'LiftedRep (Array a) #)
  go !n !xs c0 !m !s0 = withNonEmpty m s0
    (\s -> 
      let theArray :: Array a
          !theArray = reverseArrayFromListN "replicateUntilEnd" (I# n) xs
       in (# s, (# (# (# #) | #), (# | theArray #), c0 #) #)
    )
    (\_ theBytes stream s1 -> case f c0 (# | (# theBytes, stream #) #) s1 of
      (# s2, (# leftovers, res, c1 #) #) -> case res of
        (# err | #) -> (# s2, (# leftovers, (# err | #), c1 #) #)
        (# | !x #) -> go (n +# 1# ) (x : xs) c1 leftovers s2
    )

-- | Replicate the parser as long as we encounter a byte belonging to 
-- the given byte set after each run of the parser. The byte set can be
-- understood as all the bytes are considered separators. If the stream
-- ends where a separator is expected, this is considered a successful parse.
replicateIntersperseMember :: forall e c a. ByteSet -> Parser e c a -> Parser e c (Array a)
-- THIS IS BROKEN. FIX IT.
replicateIntersperseMember !set (Parser (ParserLevity f)) = Parser (ParserLevity (go 0# [])) where
  go :: Int# -> [a] -> c -> Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# e c s 'LiftedRep (Array a) #)
  go !n !xs c0 !m !s0 = withNonEmpty m s0
    (\s1 ->
        let theArray :: Array a
            !theArray = reverseArrayFromListN "replicateIntersperseMember" (I# n) xs
         in (# s1, (# (# (# #) | #), (# | theArray #), c0 #) #)
    )
    (\theByte theBytes stream s1 -> case ByteSet.member (W8# theByte) set of
      True -> case f c0 (# | (# theBytes, stream #) #) s1 of
        (# s2, (# leftovers, res, c1 #) #) -> case res of
          (# err | #) -> (# s2, (# leftovers, (# err | #), c1 #) #)
          (# | !x #) -> go (n +# 1# ) (x : xs) c1 leftovers s2
      False ->
        let theArray :: Array a
            !theArray = reverseArrayFromListN "replicateIntersperseMember" (I# n) xs
         in (# s1, (# (# | (# theBytes, stream #) #), (# | theArray #), c0 #) #)
    )

replicateIntersperseBytePrim :: forall e c a. Prim a
  => Word8 -> Parser e c a -> Parser e c (PrimArray a)
replicateIntersperseBytePrim !(W8# sepByte) (Parser (ParserLevity f)) =
  Parser (ParserLevity (onceThenReplicatePrim (ParserLevity f) go))
  where
  go :: Int# -> Int# -> MutableByteArray# s -> c -> Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# e c s 'LiftedRep (PrimArray a) #)
  go !ix !sz !marr c0 !m !s0 = withNonEmpty m s0
    (\s1 -> case shrinkMutableByteArray# marr (ix *# PM.sizeOf# (undefined :: a)) s1 of
      s2 -> case unsafeFreezeByteArray# marr s2 of
        (# s3, arr #) -> (# s3, (# (# (# #) | #), (# | PrimArray arr #), c0 #) #)
    )
    (\theByte theBytes stream s1 -> case eqWord# theByte sepByte of
      1# -> case f c0 (# | (# unsafeDrop# 1# theBytes, stream #) #) s1 of
        (# s2, (# leftovers, res, c1 #) #) -> case res of
          (# err | #) -> (# s2, (# leftovers, (# err | #), c1 #) #)
          (# | !x #) -> case ix <# sz of
            1# -> case PM.writeByteArray# marr ix x s2 of
              s3 -> go (ix +# 1# ) sz marr c1 leftovers s3
            _ -> case newByteArray# (sz *# 2# *# PM.sizeOf# (undefined :: a)) s2 of
              (# s3, marrNew #) -> case copyMutableByteArray# marr 0# marrNew 0# (sz *# PM.sizeOf# (undefined :: a)) s3 of
                s4 -> case PM.writeByteArray# marrNew ix x s4 of
                  s5 -> go (ix +# 1#) (sz *# 2#) marrNew c1 leftovers s5
      _ -> case shrinkMutableByteArray# marr (ix *# PM.sizeOf# (undefined :: a)) s1 of
        s2 -> case unsafeFreezeByteArray# marr s2 of
          (# s3, arr #) -> (# s3, (# (# | (# theBytes, stream #) #), (# | PrimArray arr #), c0 #) #)
    )

-- | Replicate the parser as long as we encounter the specified byte
-- after each run of the parser. If the stream ends where the separator
-- is expected, this is considered a successful parse.
replicateIntersperseByte :: forall e c a. Word8 -> Parser e c a -> Parser e c (Array a)
replicateIntersperseByte !(W8# sepByte) (Parser (ParserLevity f)) =
  Parser (ParserLevity (onceThenReplicate (ParserLevity f) go))
  where
  go :: Int# -> [a] -> c -> Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# e c s 'LiftedRep (Array a) #)
  go !n !xs c0 !m !s0 = withNonEmpty m s0
    (\s1 ->
        let theArray :: Array a
            !theArray = reverseArrayFromListN "replicateIntersperseByte" (I# n) xs
         in (# s1, (# (# (# #) | #), (# | theArray #), c0 #) #)
    )
    (\theByte theBytes stream s1 -> case eqWord# theByte sepByte of
      1# -> case f c0 (# | (# unsafeDrop# 1# theBytes, stream #) #) s1 of
        (# s2, (# leftovers, res, c1 #) #) -> case res of
          (# err | #) -> (# s2, (# leftovers, (# err | #), c1 #) #)
          (# | !x #) -> go (n +# 1# ) (x : xs) c1 leftovers s2
      _ -> 
        let theArray :: Array a
            !theArray = reverseArrayFromListN "replicateIntersperseByte" (I# n) xs
         in (# s1, (# (# | (# theBytes, stream #) #), (# | theArray #), c0 #) #)
    )

-- this succeeds if the input is empty 
onceThenReplicate ::
     ParserLevity e c 'LiftedRep a
  -> (Int# -> [a] -> c -> Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# e c s 'LiftedRep (Array a) #))
  -> (c -> Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# e c s 'LiftedRep (Array a) #))
onceThenReplicate (ParserLevity f) go c0 m s0 = withNonEmpty m s0
  (\s1 -> (# s1, (# (# (# #) | #), (# | mempty #), c0 #) #)
  )
  (\_ theBytes stream s1 -> case f c0 (# | (# theBytes, stream #) #) s1 of
    (# s2, (# leftovers, res, c1 #) #) -> case res of
      (# err | #) -> (# s2, (# leftovers, (# err | #), c1 #) #)
      (# | !x #) -> go 1# [x] c1 leftovers s2
  )

onceThenReplicatePrim :: forall e c s a. Prim a
  => ParserLevity e c 'LiftedRep a
  -> (Int# -> Int# -> MutableByteArray# s -> c -> Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# e c s 'LiftedRep (PrimArray a) #))
  -> (c -> Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# e c s 'LiftedRep (PrimArray a) #))
onceThenReplicatePrim (ParserLevity f) go c0 m s0 = withNonEmpty m s0
  (\s1 -> (# s1, (# (# (# #) | #), (# | mempty #), c0 #) #)
  )
  (\_ theBytes stream s1 -> case f c0 (# | (# theBytes, stream #) #) s1 of
    (# s2, (# leftovers, res, c1 #) #) -> case res of
      (# err | #) -> (# s2, (# leftovers, (# err | #), c1 #) #)
      (# | !x #) -> let sz = chooseMinElementSize (PM.sizeOf# (undefined :: a)) in
        case newByteArray# (sz *# PM.sizeOf# (undefined :: a)) s2 of
          (# s3, marr #) -> case PM.writeByteArray# marr 0# x s3 of
            s4 -> go 1# sz marr c1 leftovers s4
  )

chooseMinElementSize ::
     Int# -- element size
  -> Int#
chooseMinElementSize sz =
  case ct ># 0# of
    1# -> ct
    _ -> 1#
  where
  ct = quotInt# 64# sz


die :: String -> a
die fun = error $ "Packed.Bytes.Parser." ++ fun
  ++ ": Implmentation error in this function. Please open a bug report."

-- | This one is pretty cool. It takes a trie of parsers. It will consume
-- input until one of two cases occur:
--
-- 1. It has fully matched a key from the trie. The parser corresponding
--    to this key is then run.
-- 2. It reaches a byte that is not part of a prefix of any key in the
--    trie. The parser fails.
--
-- Like the other functions in this module, this implementation does not
-- do any backtracking. This implementation detail means that a trie
-- containing a key that is a prefix of another one of its keys will
-- shadow the longer key. For example:
--
-- > Write Example
--
trie :: Trie (Parser e c a) -> Parser e c a
trie (Trie.Trie node) = go node where
  go :: Trie.Node d (Parser e c b) -> Parser e c b
  go Trie.NodeEmpty = failure
  go (Trie.NodeValueNil p) = p
  go (Trie.NodeRun (Trie.Run arr n)) = do
    bytes (B.fromByteArray arr)
    go n
  go (Trie.NodeBranch arr) = do
    b <- any
    go (PM.indexArray arr (word8ToInt b))
  go (Trie.NodeValueRun p _) = p
  go (Trie.NodeValueBranch p _) = p

triePure :: Trie a -> Parser e c a
triePure (Trie.Trie node) = go node where
  go :: Trie.Node d b -> Parser e c b
  go Trie.NodeEmpty = failure
  go (Trie.NodeValueNil p) = pure p
  go (Trie.NodeRun (Trie.Run arr n)) = do
    bytes (B.fromByteArray arr)
    go n
  go (Trie.NodeBranch arr) = do
    b <- any
    go (PM.indexArray arr (word8ToInt b))
  go (Trie.NodeValueRun p _) = pure p
  go (Trie.NodeValueBranch p _) = pure p

-- | Variant of 'trie' whose parsers accept an environment. This is helpful
-- in situations where the user needs to ensure that the 'Trie' used by
-- the parser is a CAF.
trieReader :: Trie (r -> Parser e c a) -> r -> Parser e c a
trieReader (Trie.Trie node) = go node where
  go :: forall e c d b t. Trie.Node d (t -> Parser e c b) -> t -> Parser e c b
  go Trie.NodeEmpty _ = failure
  go (Trie.NodeValueNil p) r = p r
  go (Trie.NodeRun (Trie.Run arr n)) r = do
    bytes (B.fromByteArray arr)
    go n r
  go (Trie.NodeBranch arr) r = do
    b <- any
    go (PM.indexArray arr (word8ToInt b)) r
  go (Trie.NodeValueRun p _) r = p r
  go (Trie.NodeValueBranch p _) r = p r

-- | Variant of 'trie' whose parsers modify state and accept an environment.
trieReaderState :: Trie (r -> s -> Parser e c (s, a)) -> r -> s -> Parser e c (s,a)
trieReaderState (Trie.Trie node) = go node where
  go :: forall e c d b r' s'. Trie.Node d (r' -> s' -> Parser e c (s',b)) -> r' -> s' -> Parser e c (s',b)
  go Trie.NodeEmpty _ _ = failure
  go (Trie.NodeValueNil p) r s = p r s
  go (Trie.NodeRun (Trie.Run arr n)) r s = do
    bytes (B.fromByteArray arr)
    go n r s
  go (Trie.NodeBranch arr) r s = do
    b <- any
    go (PM.indexArray arr (word8ToInt b)) r s
  go (Trie.NodeValueRun p _) r s = p r s
  go (Trie.NodeValueBranch p _) r s = p r s

-- | Variant of 'trie' whose parsers modify state and accept an environment.
-- This variant lacks a result type. It only modifies the state.
trieReaderState_ :: Trie (r -> s -> Parser e c s) -> r -> s -> Parser e c s
trieReaderState_ (Trie.Trie node) = go node where
  go :: forall e c d r' s'. Trie.Node d (r' -> s' -> Parser e c s') -> r' -> s' -> Parser e c s'
  go Trie.NodeEmpty _ _ = failure
  go (Trie.NodeValueNil p) r s = p r s
  go (Trie.NodeRun (Trie.Run arr n)) r s = do
    bytes (B.fromByteArray arr)
    go n r s
  go (Trie.NodeBranch arr) r s = do
    b <- any
    go (PM.indexArray arr (word8ToInt b)) r s
  go (Trie.NodeValueRun p _) r s = p r s
  go (Trie.NodeValueBranch p _) r s = p r s


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
    let go !i (x:xs) = if debugMode && i < 0
          then die "reverseArrayFromListN"
          else do
            PM.writeArray mi i x
            go (i - 1) xs
        go !i [] = if debugMode && i /= (-1)
          then die "reverseArrayFromListN"
          else return ()
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

skipSpace :: Parser e c ()
{-# NOINLINE[2] skipSpace #-}
skipSpace = Parser skipSpaceUnboxed
{-# RULES "skipSpace-skipSpace{right}" [~2] sequenceRight skipSpace skipSpace = skipSpace #-}
{-# RULES "skipSpace-skipSpace{left}" [~2] sequenceLeft skipSpace skipSpace = skipSpace #-}

-- | Parse an ascii-encoded number in decimal notation. This
-- will succeed even for numbers that are too big to fit into
-- a machine word.
decimalWord :: Parser e c Word
{-# NOINLINE[2] decimalWord #-}
decimalWord = Parser (boxWordParser decimalWordUnboxed)
{-# RULES "decimalWord-unused{right}" [~2] forall a. sequenceRight decimalWord a = sequenceRight skipDigits a #-}
{-# RULES "decimalWord-unused{left}" [~2] forall a. sequenceLeft a decimalWord = sequenceLeft a skipDigits #-}

decimalInt :: Parser e c Int
decimalInt = do
  hasMinus <- optionalByte (charToWord8 '-')
  w <- decimalWord
  let i = wordToInt w
  return (if hasMinus then negate i else i)

-- | Parse a decimal word, attaching the provided value (should be
-- between 0 and 9 inclusive) to the front of it.
decimalWordStarting :: Word -> Parser e c Word
decimalWordStarting (W# w0) = Parser (boxWordParser (decimalContinue w0))

decimalDigitWord :: Parser e c Word
decimalDigitWord = Parser (boxWordParser decimalDigit)

optionalDecimalDigitWord :: Parser e c (Maybe Word)
optionalDecimalDigitWord = Parser (boxMaybeWordParser optionalDecimalDigit)

-- | Run a stateful parser.
statefully :: (forall s. StatefulParser e c s a) -> Parser e c a
statefully x = Parser (ParserLevity (case x of {StatefulParser f -> f}))

-- | Lift a 'ST' action into a stateful parser.
mutation :: ST s a -> StatefulParser e c s a
mutation (ST f) = StatefulParser $ \c0 leftovers0 s0 ->
  case f s0 of
    (# s1, a #) -> (# s1, (# leftovers0, (# | a #), c0 #) #)

-- | Lift a pure parser into a stateful parser.
consumption :: Parser e c a -> StatefulParser e c s a
consumption (Parser (ParserLevity f)) = StatefulParser f

-- TODO: improve this
mapParser :: (a -> b) -> Parser e c a -> Parser e c b
mapParser f p = bindLifted p (pureParser . f)

pureParser :: a -> Parser e c a
pureParser a = Parser $ ParserLevity $ \c0 leftovers0 s0 ->
  (# s0, (# leftovers0, (# | a #), c0 #) #)

-- TODO: improve this
mapStatefulParser :: (a -> b) -> StatefulParser e c s a -> StatefulParser e c s b
mapStatefulParser f p = bindStateful p (pureStatefulParser . f)

pureStatefulParser :: a -> StatefulParser e c s a
pureStatefulParser a = StatefulParser $ \c0 leftovers0 s0 ->
  (# s0, (# leftovers0, (# | a #), c0 #) #)

bindStateful :: StatefulParser e c s a -> (a -> StatefulParser e c s b) -> StatefulParser e c s b
bindStateful (StatefulParser f) g = StatefulParser $ \c0 leftovers0 s0 -> case f c0 leftovers0 s0 of
  (# s1, (# leftovers1, val, c1 #) #) -> case val of
    (# err | #) -> (# s1, (# leftovers1, (# err | #), c1 #) #)
    (# | x #) -> case g x of
      StatefulParser k -> k c1 leftovers1 s1

bindLifted :: Parser e c a -> (a -> Parser e c b) -> Parser e c b
bindLifted (Parser (ParserLevity f)) g = Parser $ ParserLevity $ \c0 leftovers0 s0 -> case f c0 leftovers0 s0 of
  (# s1, (# leftovers1, val, c1 #) #) -> case val of
    (# err | #) -> (# s1, (# leftovers1, (# err | #), c1 #) #)
    (# | x #) -> case g x of
      Parser (ParserLevity k) -> k c1 leftovers1 s1

bindBoxed :: ParserLevity e c 'LiftedRep a -> (a -> ParserLevity e c 'LiftedRep b) -> ParserLevity e c 'LiftedRep b
bindBoxed (ParserLevity f) g = ParserLevity $ \c0 leftovers0 s0 -> case f c0 leftovers0 s0 of
  (# s1, (# leftovers1, val, c1 #) #) -> case val of
    (# err | #) -> (# s1, (# leftovers1, (# err | #), c1 #) #)
    (# | x #) -> case g x of
      ParserLevity k -> k c1 leftovers1 s1


bindWord :: ParserLevity e c 'WordRep Word# -> (Word# -> ParserLevity e c 'WordRep Word#) -> ParserLevity e c 'WordRep Word#
bindWord (ParserLevity f) g = ParserLevity $ \c0 leftovers0 s0 -> case f c0 leftovers0 s0 of
  (# s1, (# leftovers1, val, c1 #) #) -> case val of
    (# err | #) -> (# s1, (# leftovers1, (# err | #), c1 #) #)
    (# | x #) -> case g x of
      ParserLevity k -> k c1 leftovers1 s1

boxWord8Parser :: ParserLevity e c 'WordRep Word# -> ParserLevity e c 'LiftedRep Word8
boxWord8Parser p = ParserLevity $ \c0 leftovers0 s0 ->
  case getParserLevity p c0 leftovers0 s0 of
    (# s1, (# leftovers1, val, c1 #) #) -> case val of
      (# err | #) -> (# s1, (# leftovers1, (# err | #), c1 #) #)
      (# | x #) -> (# s1, (# leftovers1, (# | W8# x #), c1 #) #)

boxWordParser :: ParserLevity e c 'WordRep Word# -> ParserLevity e c 'LiftedRep Word
boxWordParser p = ParserLevity $ \c0 leftovers0 s0 ->
  case getParserLevity p c0 leftovers0 s0 of
    (# s1, (# leftovers1, val, c1 #) #) -> case val of
      (# err | #) -> (# s1, (# leftovers1, (# err | #), c1 #) #)
      (# | x #) -> (# s1, (# leftovers1, (# | W# x #), c1 #) #)

boxMaybeWordParser ::
     ParserLevity e c ('SumRep '[ 'TupleRep '[], 'WordRep]) (Maybe# Word#)
  -> ParserLevity e c 'LiftedRep (Maybe Word)
boxMaybeWordParser p = ParserLevity $ \c0 leftovers0 s0 ->
  case getParserLevity p c0 leftovers0 s0 of
    (# s1, (# leftovers1, val, c1 #) #) -> case val of
      (# err | #) -> (# s1, (# leftovers1, (# err | #), c1 #) #)
      (# | m #) -> case m of
         (# (# #) | #) -> (# s1, (# leftovers1, (# | Nothing #), c1 #) #)
         (# | x #) -> (# s1, (# leftovers1, (# | Just (W# x) #), c1 #) #)

boxBytesWord8Parser ::
     ParserLevity e c ('TupleRep '[BytesRep, 'WordRep]) (# Bytes#, Word# #)
  -> ParserLevity e c 'LiftedRep (Bytes,Word8)
boxBytesWord8Parser p = ParserLevity $ \c0 leftovers0 s0 ->
  case getParserLevity p c0 leftovers0 s0 of
    (# s1, (# leftovers1, val, c1 #) #) -> case val of
      (# err | #) -> (# s1, (# leftovers1, (# err | #), c1 #) #)
      (# | (# theBytes, theWord #) #) -> (# s1, (# leftovers1, (# | (boxBytes theBytes, W8# theWord) #), c1 #) #)

boxBytesParser ::
     ParserLevity e c BytesRep Bytes#
  -> ParserLevity e c 'LiftedRep Bytes
boxBytesParser p = ParserLevity $ \c0 leftovers0 s0 ->
  case getParserLevity p c0 leftovers0 s0 of
    (# s1, (# leftovers1, val, c1 #) #) -> case val of
      (# err | #) -> (# s1, (# leftovers1, (# err | #), c1 #) #)
      (# | theBytes #) -> (# s1, (# leftovers1, (# | boxBytes theBytes #), c1 #) #)

unboxWord :: Word -> Word#
unboxWord (W# i) = i

-- This assumes that the Bytes is longer than the index. It also does
-- not eliminate zero-length references to byte arrays.
unsafeDrop# :: Int# -> Bytes# -> Bytes#
unsafeDrop# i (# arr, off, len #) = (# arr, off +# i, len -# i #)

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
endOfLine :: Parser e c ()
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

failure :: Parser e c a
failure = Parser (ParserLevity (\c m s -> (# s, (# m, (# Nothing | #), c #) #)))

-- | Modify the context used for errors. The modification is not
-- reset or undone at any point.
modifyContext :: (c -> c) -> Parser e c a -> Parser e c a
modifyContext f (Parser (ParserLevity p)) = Parser $ ParserLevity
  $ \c0 leftovers0 s0 -> p (f c0) leftovers0 s0

-- | Modify the context used for errors. If the parser given as
-- an argument completes, the context is reset.
scoped :: (c -> c) -> Parser e c a -> Parser e c a
scoped f (Parser (ParserLevity p)) = Parser $ ParserLevity $ \c0 leftovers0 s0 ->
  case p (f c0) leftovers0 s0 of
    (# s1, (# leftovers1, val@(# _ | #), c1 #) #) -> (# s1, (# leftovers1, val, c1 #) #)
    (# s1, (# leftovers1, val@(# | _ #), _ #) #) -> (# s1, (# leftovers1, val, c0 #) #)

word8ToInt :: Word8 -> Int
word8ToInt = fromIntegral

charToWord8 :: Char -> Word8
charToWord8 = fromIntegral . ord

