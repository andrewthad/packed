{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
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

module Packed.Bytes.Stream.Parser
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
  , bigEndianWord32
  , bigEndianWord64
  , bigEndianFloat
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
  , replicateIndex#
  , replicateUntilEnd
  -- , replicateIntersperseUntilEnd
  -- , replicateUntilByte
  , replicateIntersperseByte
  , replicateIntersperseByteIndex#
  , replicateIntersperseMember
  , replicateIntersperseBytePrim
  , foldIntersperseParserUntilEnd
  , foldIntersperseByte
  , trie
  , triePure
  -- , trieReader
  -- , trieReaderState
  -- , trieReaderState_
  , failure
    -- * Stateful
  , statefully
  , consumption
  , mutation
    -- * ASCII
  , skipSpace
  , endOfLine
  , takeBytesUntilEndOfLineConsume
  , charUtf8
  ) where

import Prelude hiding (any,replicate)

import Control.Applicative
import Control.Monad (when,(>=>))
import Control.Monad.ST (runST)
import Control.Monad.Primitive (PrimMonad(..))
import Data.Bits ((.&.),(.|.),unsafeShiftL,xor)
import Data.Char (ord)
import Data.Primitive (Prim,Array(..),MutableArray(..),PrimArray(..))
import Data.Semigroup (Semigroup)
import GHC.Base (Char(C#))
import GHC.Int (Int(I#))
import GHC.ST (ST(..))
import GHC.Types (TYPE,RuntimeRep(..),IO(..))
import GHC.Word (Word(W#),Word8(W8#),Word32(W32#),Word64(W64#))
import GHC.Float (Float(F#))
import Packed.Bytes (Bytes(..))
import Packed.Bytes.Set (ByteSet)
import Packed.Bytes.Small (ByteArray(..))
import Packed.Bytes.Stream.ST (ByteStream(..))
import Packed.Bytes.Trie (Trie)

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
  unsafeFreezeByteArray#,newByteArray#,
  plusWord#,timesWord#,indexWord8Array#,eqWord#,andI#,
  clz8#, or#, neWord#, uncheckedShiftL#,int2Word#,word2Int#,quotInt#,
  shrinkMutableByteArray#,copyMutableByteArray#,chr#,
  writeWord32Array#,readFloatArray#,runRW#,
  RealWorld)

type Bytes# = (# ByteArray#, Int#, Int# #)
type Maybe# (a :: TYPE r) = (# (# #) | a #)
type Either# a (b :: TYPE r) = (# a | b #)
type Leftovers# s = (# Bytes# , ByteStream s #)
type Result# e s (r :: RuntimeRep) (a :: TYPE r) =
  (# Maybe# (Leftovers# s), Either# e a #)
type BytesRep = 'TupleRep '[ 'UnliftedRep, 'IntRep, 'IntRep ]

debugMode :: Bool
debugMode = False

data Result e s a = Result
  { resultLeftovers :: !(Maybe (Leftovers s))
  , resultValue :: !(Either e a)
  }

data Leftovers s = Leftovers
  { leftoversChunk :: {-# UNPACK #-} !Bytes
    -- ^ The last chunk pulled from the stream
  , leftoversStream :: ByteStream s
    -- ^ The remaining stream
  }

data PureResult e a = PureResult
  { pureResultLeftovers :: {-# UNPACK #-} !Bytes
  , pureResultValue :: !(Either e a)
  } deriving (Show,Eq)

parseBytes :: Bytes -> Parser e a -> PureResult e a
parseBytes theBytes p = runST $ do
  Result mleftovers mval <- parseStreamST (Stream.fromBytes theBytes) p
  theLeftovers <- case mleftovers of
    Nothing -> return B.empty
    Just (Leftovers chunk stream) -> do
      others <- Stream.toBytes stream
      return (B.append chunk others)
  return (PureResult theLeftovers mval)

parseStreamST :: ByteStream s -> Parser e a -> ST s (Result e s a)
parseStreamST stream (Parser (ParserLevity f)) = ST $ \s0 ->
  case f (# | (# (# unboxByteArray BA.empty, 0#, 0# #), stream #) #) s0 of
    (# s1, r #) -> (# s1, boxResult r #)

parseStreamIO :: ByteStream RealWorld -> Parser e a -> IO (Result e RealWorld a)
parseStreamIO stream (Parser (ParserLevity f)) = IO $ \s0 ->
  case f (# | (# (# unboxByteArray BA.empty, 0#, 0# #), stream #) #) s0 of
    (# s1, r #) -> (# s1, boxResult r #)

boxResult :: Result# e s 'LiftedRep a -> Result e s a
boxResult (# leftovers, val #) = case val of
  (# err | #) -> Result (boxLeftovers leftovers) (Left err)
  (# | a #) -> Result (boxLeftovers leftovers) (Right a)

boxLeftovers :: Maybe# (Leftovers# s) -> Maybe (Leftovers s)
boxLeftovers (# (# #) | #) = Nothing
boxLeftovers (# | (# theBytes, stream #) #) = Just (Leftovers (boxBytes theBytes) stream)

newtype Parser e a = Parser { getParser :: ParserLevity e 'LiftedRep a }

instance Functor (Parser e) where
  fmap = mapParser

-- Remember to write liftA2 by hand at some point.
instance Applicative (Parser e) where
  pure = pureParser
  (<*>) = Control.Monad.ap
  (*>) = sequenceRight
  (<*) = sequenceLeft
  {-# INLINE (*>) #-}
  {-# INLINE (<*) #-}

instance Monad (Parser e) where
  return = pure
  (>>=) = bindLifted

instance Semigroup a => Semigroup (Parser e a) where
  (<>) = liftA2 (SG.<>)

instance Monoid a => Monoid (Parser e a) where
  mempty = pure mempty

instance Functor (StatefulParser e s) where
  fmap = mapStatefulParser

instance Applicative (StatefulParser e s) where
  pure = pureStatefulParser
  (<*>) = Control.Monad.ap

instance Monad (StatefulParser e s) where
  return = pure
  (>>=) = bindStateful

instance Semigroup a => Semigroup (StatefulParser e s a) where
  (<>) = liftA2 (SG.<>)

instance Monoid a => Monoid (StatefulParser e s a) where
  mempty = pure mempty


newtype ParserLevity e (r :: RuntimeRep) (a :: TYPE r) = ParserLevity
  { getParserLevity :: forall s.
       Maybe# (Leftovers# s)
    -> State# s
    -> (# State# s, Result# e s r a #)
  }

-- | A parser that can interleave arbitrary 'ST' effects with parsing.
newtype StatefulParser e s a = StatefulParser
  { getStatefulParser ::
       Maybe# (Leftovers# s)
    -> State# s
    -> (# State# s, Result# e s 'LiftedRep a #)
  }

instance PrimMonad (StatefulParser e s) where
  type PrimState (StatefulParser e s) = s
  primitive f = StatefulParser
    (\m s0 -> case f s0 of
      (# s1, a #) -> (# s1, (# m, (# | a #) #) #)
    )

sequenceRight :: Parser e a -> Parser e b -> Parser e b
{-# NOINLINE[2] sequenceRight #-}
sequenceRight (Parser (ParserLevity f)) (Parser (ParserLevity g)) =
  Parser $ ParserLevity $ \leftovers0 s0 -> case f leftovers0 s0 of
    (# s1, (# leftovers1, val #) #) -> case val of
      (# r | #) -> (# s1, (# leftovers1, (# r | #) #) #)
      (# | _ #) -> g leftovers1 s1

sequenceLeft :: Parser e a -> Parser e b -> Parser e a
{-# NOINLINE[2] sequenceLeft #-}
sequenceLeft (Parser (ParserLevity f)) (Parser (ParserLevity g)) =
  Parser $ ParserLevity $ \leftovers0 s0 -> case f leftovers0 s0 of
    (# s1, (# leftovers1, val #) #) -> case val of
      (# r | #) -> (# s1, (# leftovers1, (# r | #) #) #)
      (# | a #) -> case g leftovers1 s1 of
        (# s2, (# leftovers2, val2 #) #) -> case val2 of
          (# r | #) -> (# s2, (# leftovers2, (# r | #) #) #)
          (# | _ #) -> (# s2, (# leftovers2, (# | a #) #) #)

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
withNonEmpty :: forall e s (r :: RuntimeRep) (b :: TYPE r).
     Maybe# (Leftovers# s)
  -> State# s
  -> (State# s -> (# State# s, Result# e s r b #))
  -> (Word# -> Bytes# -> ByteStream s -> State# s -> (# State# s, Result# e s r b #))
     -- The first argument is a Word8, not a full machine word.
     -- The second argument is the complete,non-empty chunk
     -- with the head byte still intact.
  -> (# State# s, Result# e s r b #)
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
withAtLeast :: forall e s (r :: RuntimeRep) (b :: TYPE r).
     Maybe# (Leftovers# s)
  -> Int# -- number of bytes to require
  -> State# s
  -> (Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# e s r b #))
     -- This action is taken if not enough bytes are present
     -- or if the stream is empty.
  -> (Bytes# -> ByteStream s -> State# s -> (# State# s, Result# e s r b #))
     -- The first argument is the complete,non-empty chunk
     -- with at least n bytes present. This action is taken
     -- if at least the right number of bytes were present.
  -> (# State# s, Result# e s r b #)
{-# INLINE withAtLeast #-}
withAtLeast (# (# #) | #) _ s0 g _ = g (# (# #) | #) s0
withAtLeast (# | (# bytes0@(# _,_,len0 #), stream0 #) #) n s0 g f = case len0 >=# n of
  1# -> f bytes0 stream0 s0
  _ -> g (# | (# bytes0, stream0 #) #) s0

decimalDigit :: e -> ParserLevity e 'WordRep Word#
decimalDigit e = ParserLevity $ \leftovers0 s0 -> case leftovers0 of
  (# (# #) | #) -> (# s0, (# (# (# #) | #), (# e | #) #) #)
  (# | (# bytes0@(# _,_,len #), stream0 #) #) ->
    let !(# s1, r #) = case len of
          0# -> nextNonEmpty stream0 s0
          _ -> (# s0, (# | (# bytes0, stream0 #) #) #)
     in case r of
          (# (# #) | #) -> (# s1, (# (# (# #) | #), (# e | #) #) #)
          (# | (# bytes1, stream1 #) #) ->
            let !w = word8ToWord (bytesIndex bytes1 0) - 48
             in if w < 10
                  then (# s1, (# (# | (# unsafeDrop# 1# bytes1, stream1 #) #), (# | unboxWord w #) #) #)
                  else (# s1, (# (# | (# bytes1, stream1 #) #), (# e | #) #) #)

optionalDecimalDigit :: ParserLevity e ('SumRep '[ 'TupleRep '[], 'WordRep]) (Maybe# Word#)
optionalDecimalDigit = ParserLevity $ \leftovers0 s0 -> case leftovers0 of
  (# (# #) | #) -> (# s0, (# (# (# #) | #), (# | (# (# #) | #) #) #) #)
  (# | (# bytes0@(# _,_,len #), stream0 #) #) ->
    let !(# s1, r #) = case len of
          0# -> nextNonEmpty stream0 s0
          _ -> (# s0, (# | (# bytes0, stream0 #) #) #)
     in case r of
          (# (# #) | #) -> (# s1, (# (# (# #) | #), (# | (# (# #) | #) #) #) #)
          (# | (# bytes1, stream1 #) #) ->
            let !w = word8ToWord (bytesIndex bytes1 0) - 48
             in if w < 10
                  then (# s1, (# (# | (# unsafeDrop# 1# bytes1, stream1 #) #), (# | (# | unboxWord w #) #) #) #)
                  else (# s1, (# (# | (# bytes1, stream1 #) #), (# | (# (# #) | #) #) #) #)

decimalContinue :: Word# -> ParserLevity e 'WordRep Word#
decimalContinue theWord = ParserLevity (action theWord) where
  action :: Word# -> Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# e s 'WordRep Word# #)
  action !w0 (# (# #) | #) s0 = (# s0, (# (# (# #) | #), (# | w0 #) #) #)
  action !w0 (# | leftovers0 #) s0 = go w0 leftovers0 s0
  go :: Word# -> Leftovers# s -> State# s -> (# State# s, Result# e s 'WordRep Word# #)
  go !w0 leftovers0@(# (# _,_,len #), !stream0 #) s0 = 
    let !(# s1, leftovers1 #) = case len of
          0# -> nextNonEmpty stream0 s0
          _ -> (# s0, (# | leftovers0 #) #)
     in case leftovers1 of
          (# (# #) | #) -> (# s1, (# (# (# #) | #), (# | w0 #) #) #)
          (# | (# bytes1, stream1 #) #) ->
            let !w = word8ToWord (bytesIndex bytes1 0) - 48
             in if w < 10
                  then go (plusWord# (timesWord# w0 10##) (unboxWord w)) (# unsafeDrop# 1# bytes1, stream1 #) s1
                  else (# s1, (# (# | (# bytes1, stream1 #) #), (# | w0 #) #) #)

decimalWordUnboxed :: e -> ParserLevity e 'WordRep Word#
decimalWordUnboxed e = bindWord (decimalDigit e) decimalContinue

bigEndianWord32 :: e -> Parser e Word32
bigEndianWord32 e = Parser (boxWord32Parser (bigEndianWord32Unboxed e))

bigEndianWord32Unboxed :: forall e. e -> ParserLevity e 'WordRep Word#
bigEndianWord32Unboxed e = ParserLevity action where
  action :: Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# e s 'WordRep Word# #)
  action m0 s0 = withAtLeast m0 4# s0
    (getParserLevity
      ( anyUnboxed e `bindWord` \byteA ->
        anyUnboxed e `bindWord` \byteB ->
        anyUnboxed e `bindWord` \byteC ->
        anyUnboxed e `bindWord` \byteD ->
        let !theWord = uncheckedShiftL# byteA 24#
                 `or#` uncheckedShiftL# byteB 16#
                 `or#` uncheckedShiftL# byteC 8#
                 `or#` byteD
         in pureWord theWord
      )
    )
    (\bytes0@(# arr, off, _ #) stream s1 ->
      let !byteA = indexWord8Array# arr (off +# 0#)
          !byteB = indexWord8Array# arr (off +# 1#)
          !byteC = indexWord8Array# arr (off +# 2#)
          !byteD = indexWord8Array# arr (off +# 3#)
          !theWord = uncheckedShiftL# byteA 24#
               `or#` uncheckedShiftL# byteB 16#
               `or#` uncheckedShiftL# byteC 8#
               `or#` byteD
       in (# s1, (# (# | (# unsafeDrop# 4# bytes0, stream #) #), (# | theWord #) #) #)
    )

-- TODO: make this compile on 32-bit architectures
bigEndianWord64 :: forall e. e -> Parser e Word64
bigEndianWord64 e = Parser (ParserLevity action) where
  action :: Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# e s 'LiftedRep Word64 #)
  action m0 s0 = withAtLeast m0 8# s0
    (let go :: Int -> Word64 -> Parser e Word64
         go !ix !r = if ix < 8
           then do
             theByte <- any e
             go (ix + 1) (unsafeShiftL r 8 .|. (fromIntegral theByte :: Word64))
           else pure r
      in getParserLevity (getParser (go 0 0))
    )
    (\bytes0@(# arr, off, _ #) stream s1 ->
      let !byteA = indexWord8Array# arr (off +# 0#)
          !byteB = indexWord8Array# arr (off +# 1#)
          !byteC = indexWord8Array# arr (off +# 2#)
          !byteD = indexWord8Array# arr (off +# 3#)
          !byteE = indexWord8Array# arr (off +# 4#)
          !byteF = indexWord8Array# arr (off +# 5#)
          !byteG = indexWord8Array# arr (off +# 6#)
          !byteH = indexWord8Array# arr (off +# 7#)
          !theWord = uncheckedShiftL# byteA 56#
              `or#` uncheckedShiftL# byteB 48#
              `or#` uncheckedShiftL# byteC 40#
              `or#` uncheckedShiftL# byteD 32#
              `or#` uncheckedShiftL# byteE 24#
              `or#` uncheckedShiftL# byteF 16#
              `or#` uncheckedShiftL# byteG 8#
              `or#` byteH
       in (# s1, (# (# | (# unsafeDrop# 8# bytes0, stream #) #), (# | W64# theWord #) #) #)
    )
bigEndianFloat :: e -> Parser e Float
bigEndianFloat e = fmap word32ToFloat (bigEndianWord32 e)

word32ToFloat :: Word32 -> Float
word32ToFloat (W32# w) = F#
  (runRW#
    (\s0 -> case newByteArray# 4# s0 of
      (# s1, arr #) -> case writeWord32Array# arr 0# w s1 of
        s2 -> case readFloatArray# arr 0# s2 of
          (# _, f #) -> f
    )
  )

skipSpaceUnboxed :: ParserLevity e 'LiftedRep ()
skipSpaceUnboxed = ParserLevity go where
  go :: Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# e s 'LiftedRep () #)
  go (# (# #) | #) s0 = (# s0, (# (# (# #) | #), (# | () #) #) #)
  go (# | (# bytes0@(# arr, off, len #), !stream0@(ByteStream streamFunc) #) #) s0 = case BAW.findNonAsciiSpace' (I# off) (I# len) (ByteArray arr) of
    (# (# #) | #) -> case streamFunc s0 of
      (# s1, r #) -> go r s1
    (# | ix #) -> (# s0, (# (# | (# unsafeDrop# (ix -# off) bytes0, stream0 #) #), (# | () #) #) #)

-- | Takes all 'Bytes' until the first occurrence of a byte in the
--   provided set. The bytes leading up to it are returned as the
--   first element of the tuple. The matching byte itself is consumed
--   from the input and returned as the second element of the tuple. This
--   parser will fail if the input ends before a byte from the set is
--   encountered.
{-# INLINE takeBytesUntilMemberConsume #-}
takeBytesUntilMemberConsume :: e -> ByteSet -> Parser e (Bytes,Word8)
takeBytesUntilMemberConsume e (ByteSet.ByteSet (ByteArray set)) = Parser (boxBytesWord8Parser (takeBytesUntilMemberConsumeUnboxed e set))

{-# NOINLINE takeBytesUntilMemberConsumeUnboxed #-}
takeBytesUntilMemberConsumeUnboxed :: forall e. e -> ByteArray# -> ParserLevity e ('TupleRep '[BytesRep, 'WordRep]) (# Bytes#, Word# #)
takeBytesUntilMemberConsumeUnboxed e !set = ParserLevity (go (# (# #) | #)) where
  go :: Maybe# Bytes# -> Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# e s ('TupleRep '[BytesRep, 'WordRep]) (# Bytes#, Word# #) #)
  go !_ (# (# #) | #) s0 = (# s0, (# (# (# #) | #), (# e | #) #) #)
  go !mbytes (# | (# bytes0@(# arr, off, len #), !stream0@(ByteStream streamFunc) #) #) s0 = case BAW.findMemberByte (I# off) (I# len) (ByteSet.ByteSet (ByteArray set)) (ByteArray arr) of
    Nothing -> case streamFunc s0 of
      (# s1, r #) -> go (# | appendMaybeBytes mbytes bytes0 #) r s1
    Just (I# ix, W8# w) -> (# s0, (# (# | (# unsafeDrop# ((ix -# off) +# 1# ) bytes0, stream0 #) #), (# | (# appendMaybeBytes mbytes (# arr, off, ix -# off #), w #) #) #) #)

-- | Takes all 'Bytes' until the first occurrence of the given byte.
--   The bytes leading up to it are returned, and the matching byte
--   itself is consumed from the input.
{-# INLINE takeBytesUntilByteConsume #-}
takeBytesUntilByteConsume :: e -> Word8 -> Parser e Bytes
takeBytesUntilByteConsume e (W8# theByte) = Parser (boxBytesParser (takeBytesUntilByteConsumeUnboxed e theByte))

{-# NOINLINE takeBytesUntilByteConsumeUnboxed #-}
takeBytesUntilByteConsumeUnboxed :: forall e. e -> Word# -> ParserLevity e BytesRep Bytes#
takeBytesUntilByteConsumeUnboxed e !theByte = ParserLevity (go (# (# #) | #)) where
  go :: Maybe# Bytes# -> Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# e s BytesRep Bytes# #)
  go !_ (# (# #) | #) s0 = (# s0, (# (# (# #) | #), (# e | #) #) #)
  go !mbytes (# | (# bytes0@(# arr, off, len #), !stream0@(ByteStream streamFunc) #) #) s0 = case BAW.findByte (I# off) (I# len) (W8# theByte) (ByteArray arr) of
    Nothing -> case streamFunc s0 of
      (# s1, r #) -> go (# | appendMaybeBytes mbytes bytes0 #) r s1
    Just (I# ix) -> (# s0, (# (# | (# unsafeDrop# ((ix -# off) +# 1# ) bytes0, stream0 #) #), (# | appendMaybeBytes mbytes (# arr, off, ix -# off #) #) #) #)

-- | Takes all 'Bytes' until the first occurrence of the given byte.
--   The bytes leading up to it are returned, and the matching byte
--   itself remains part of the input.
{-# INLINE takeBytesUntilByte #-}
takeBytesUntilByte :: e -> Word8 -> Parser e Bytes
takeBytesUntilByte e (W8# theByte) = Parser (boxBytesParser (takeBytesUntilByteUnboxed e theByte))

{-# NOINLINE takeBytesUntilByteUnboxed #-}
takeBytesUntilByteUnboxed :: forall e. e -> Word# -> ParserLevity e BytesRep Bytes#
-- fix this. It should succeed if it reaches the end of the input.
-- Also, the error value is unneeded.
takeBytesUntilByteUnboxed e !theByte = ParserLevity (go (# (# #) | #)) where
  go :: Maybe# Bytes# -> Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# e s BytesRep Bytes# #)
  go !_ (# (# #) | #) s0 = (# s0, (# (# (# #) | #), (# e | #) #) #)
  go !mbytes (# | (# bytes0@(# arr, off, len #), !stream0@(ByteStream streamFunc) #) #) s0 = case BAW.findByte (I# off) (I# len) (W8# theByte) (ByteArray arr) of
    Nothing -> case streamFunc s0 of
      (# s1, r #) -> go (# | appendMaybeBytes mbytes bytes0 #) r s1
    Just (I# ix) -> (# s0, (# (# | (# unsafeDrop# (ix -# off) bytes0, stream0 #) #), (# | appendMaybeBytes mbytes (# arr, off, ix -# off #) #) #) #)

skipDigits :: Parser e ()
skipDigits = Parser (ParserLevity go) where
  go :: Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# e s 'LiftedRep () #)
  go (# (# #) | #) s0 = (# s0, (# (# (# #) | #), (# | () #) #) #)
  go (# | (# bytes0@(# arr, off, len #), !stream0@(ByteStream streamFunc) #) #) s0 = case BAW.findNonDigit (I# off) (I# len) (ByteArray arr) of
    Nothing -> case streamFunc s0 of
      (# s1, r #) -> go r s1
    Just (I# ix, _) -> (# s0, (# (# | (# unsafeDrop# (ix -# off) bytes0, stream0 #) #), (# | () #) #) #)

-- | Skip all bytes until the given byte is encountered. Then, consume
-- the byte. This parser will fail if the end of the input is encountered.
skipUntilByteConsume :: e -> Word8 -> Parser e ()
skipUntilByteConsume e (W8# w) = Parser (skipUntilByteConsumeUnboxed e w)

skipUntilByteConsumeUnboxed :: forall e. e -> Word# -> ParserLevity e 'LiftedRep ()
skipUntilByteConsumeUnboxed e !theByte = ParserLevity go where
  go :: Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# e s 'LiftedRep () #)
  go (# (# #) | #) s0 = (# s0, (# (# (# #) | #), (# e | #) #) #)
  go (# | (# bytes0@(# arr, off, len #), !stream0@(ByteStream streamFunc) #) #) s0 = case BAW.findByte (I# off) (I# len) (W8# theByte) (ByteArray arr) of
    Nothing -> case streamFunc s0 of
      (# s1, r #) -> go r s1
    Just (I# ix) -> (# s0, (# (# | (# unsafeDrop# ((ix -# off) +# 1# ) bytes0, stream0 #) #), (# | () #) #) #)

-- | Skips all bytes matching the given byte.
skipWhileByte :: e -> Word8 -> Parser e ()
-- TODO: Improve the performance of this. I suspect that we can travel a full machine word at a time.
-- We will need to add something to Packed.Bytes.Window to make this work.
-- Also, remove the error value at some point.
skipWhileByte e theByte = go where
  go = isEndOfInput >>= \case
    True -> return ()
    False -> do
      b <- peek e
      if b == theByte
        then any e >> go
        else return ()

skipUntilByte :: Word8 -> Parser e ()
skipUntilByte (W8# theByte) = Parser (ParserLevity go) where
  go :: Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# e s 'LiftedRep () #)
  go (# (# #) | #) s0 = (# s0, (# (# (# #) | #), (# | () #) #) #)
  go (# | (# bytes0@(# arr, off, len #), !stream0@(ByteStream streamFunc) #) #) s0 = case BAW.findByte (I# off) (I# len) (W8# theByte) (ByteArray arr) of
    Nothing -> case streamFunc s0 of
      (# s1, r #) -> go r s1
    Just (I# ix) -> (# s0, (# (# | (# unsafeDrop# (ix -# off) bytes0, stream0 #) #), (# | () #) #) #)

{-# INLINE takeBytesUntilEndOfLineConsume #-}
takeBytesUntilEndOfLineConsume :: e -> Parser e Bytes
takeBytesUntilEndOfLineConsume e = Parser (boxBytesParser (takeBytesUntilEndOfLineConsumeUnboxed e))

{-# NOINLINE takeBytesUntilEndOfLineConsumeUnboxed #-}
takeBytesUntilEndOfLineConsumeUnboxed :: forall e. e -> ParserLevity e BytesRep Bytes#
takeBytesUntilEndOfLineConsumeUnboxed e = ParserLevity (go (# (# #) | #)) where
  go :: Maybe# Bytes# -> Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# e s BytesRep Bytes# #)
  go !_ (# (# #) | #) s0 = (# s0, (# (# (# #) | #), (# e | #) #) #)
  go !mbytes (# | (# bytes0@(# arr0, off0, len0 #), !stream0@(ByteStream streamFunc) #) #) s0 = case BAW.findAnyByte2 (I# off0) (I# len0) 10 13 (ByteArray arr0) of
    Nothing -> case streamFunc s0 of
      (# s1, r #) -> go (# | appendMaybeBytes mbytes bytes0 #) r s1
    Just (I# ix, W8# theByte) -> case theByte of
      10## -> (# s0, (# (# | (# unsafeDrop# ((ix -# off0) +# 1# ) bytes0, stream0 #) #), (# | appendMaybeBytes mbytes (# arr0, off0, ix -# off0 #) #) #) #)
      -- second case means it was 13
      _ -> case ix <# (off0 +# len0 -# 1#) of
        1# -> case indexWord8Array# arr0 (ix +# 1# ) of
          10## -> (# s0, (# (# | (# unsafeDrop# ((ix -# off0) +# 2# ) bytes0, stream0 #) #), (# | appendMaybeBytes mbytes (# arr0, off0, ix -# off0 #) #) #) #)
          _ -> (# s0, (# (# | (# unsafeDrop# (ix -# off0) bytes0, stream0 #) #), (# e | #) #) #)
        _ -> case nextNonEmpty stream0 s0 of
          (# s1, m #) -> case m of
            (# (# #) | #) -> (# s1, (# (# | (# unboxBytes (B.singleton 13), Stream.empty #) #), (# e | #) #) #)
            (# | (# bytes1@(# arr1, _, _ #), stream1 #) #) -> case indexWord8Array# arr1 0# of
              10## -> (# s1, (# (# | (# unsafeDrop# 1# bytes1, stream1 #) #), (# | appendMaybeBytes mbytes (# arr0, off0, ix -# off0 #) #) #) #)
              _ -> (# s1, (# (# | (# unboxBytes (B.cons 13 (boxBytes bytes1)), stream1 #) #), (# e | #) #) #)

-- | Takes all 'Bytes' until the first occurrence of a byte in the
--   provided set. The bytes leading up to it are returned. The matching
--   byte is not consumed. This parser succeeds if the input ends before
--   a byte from the set is encountered.
takeBytesWhileMember :: ByteSet -> Parser e Bytes
takeBytesWhileMember b = Parser (takeBytesWhileMemberUnboxed b)

takeBytesWhileMemberUnboxed :: ByteSet -> ParserLevity e 'LiftedRep Bytes
takeBytesWhileMemberUnboxed !set = ParserLevity (go (# (# #) | #)) where
  go :: Maybe# Bytes# -> Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# e s 'LiftedRep Bytes #)
  go mbytes (# (# #) | #) s0 = (# s0, (# (# (# #) | #), (# | maybeBytesToBytes mbytes #) #) #)
  go mbytes (# | (# bytes0@(# arr, off, len #), !stream0@(ByteStream streamFunc) #) #) s0 = case BAW.findNonMemberByte (I# off) (I# len) set (ByteArray arr) of
    Nothing -> case streamFunc s0 of
      (# s1, r #) -> go (# | appendMaybeBytes mbytes bytes0 #) r s1
    Just (I# ix,!_) -> (# s0, (# (# | (# unsafeDrop# (ix -# off) bytes0, stream0 #) #), (# | boxBytes (appendMaybeBytes mbytes (# arr, off, ix -# off #)) #) #) #)

-- Argument should be an 8-bit word.
byteUnboxed :: forall e. e -> Word# -> ParserLevity e 'LiftedRep ()
byteUnboxed e expected = ParserLevity go where
  go :: Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# e s 'LiftedRep () #)
  go m s0 = withNonEmpty m s0
    (\s -> (# s, (# (# (# #) | #), (# e | #) #) #))
    (\actual theBytes stream s -> case eqWord# expected actual of
      1# -> (# s, (# (# | (# unsafeDrop# 1# theBytes, stream #) #), (# | () #) #) #)
      _ -> (# s, (# (# | (# theBytes, stream #) #), (# e | #) #) #)
    )

-- Arguments should be 8-bit words. This should be exactly
-- equivalent to calling byteUnboxed twice.
byteTwoUnboxed :: forall e. e -> e -> Word# -> Word# -> ParserLevity e 'LiftedRep ()
byteTwoUnboxed e0 e1 expected0 expected1 = ParserLevity go where
  go :: Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# e s 'LiftedRep () #)
  go m s0 = withAtLeast m 2# s0
    (getParserLevity
      (bindBoxed (byteUnboxed e0 expected0)
        (\_ -> byteUnboxed e1 expected1)
      )
    )
    (\theBytes@(# arr, off, _ #) stream s ->
        let ax = eqWord# expected0 (indexWord8Array# arr off)
            bx = eqWord# expected1 (indexWord8Array# arr (off +# 1#))
         in case ax `andI#` bx of
          1# -> (# s, (# (# | (# unsafeDrop# 2# theBytes, stream #) #), (# | () #) #) #)
          _ ->
            let ay = int2Word# (neWord# expected0 (indexWord8Array# arr off))
                by = int2Word# (neWord# expected1 (indexWord8Array# arr (off +# 1#)))
                artifact = uncheckedShiftL# ay 1# `or#` by
                res = 2# -# (8# -# word2Int# (clz8# artifact))
                e = case res of
                  0# -> e0 
                  _ -> e1
             in (# s, (# (# | (# unsafeDrop# res theBytes, stream #) #), (# e | #) #) #)
    )

-- Arguments should be 8-bit words. This should be exactly
-- equivalent to calling byteUnboxed three times.
byteThreeUnboxed :: forall e. e -> e -> e -> Word# -> Word# -> Word# -> ParserLevity e 'LiftedRep ()
byteThreeUnboxed e0 e1 e2 expected0 expected1 expected2 = ParserLevity go where
  go :: Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# e s 'LiftedRep () #)
  go m s0 = withAtLeast m 3# s0
    (getParserLevity
      (bindBoxed (byteUnboxed e0 expected0)
        (\_ -> bindBoxed (byteUnboxed e1 expected1)
          (\_ -> byteUnboxed e2 expected2)
        )
      )
    )
    (\theBytes@(# arr, off, _ #) stream s ->
        let aw = indexWord8Array# arr off
            bw = indexWord8Array# arr (off +# 1#)
            cw = indexWord8Array# arr (off +# 2#)
            ax = eqWord# expected0 aw
            bx = eqWord# expected1 bw
            cx = eqWord# expected2 cw
         in case ax `andI#` bx `andI#` cx of
          1# -> (# s, (# (# | (# unsafeDrop# 3# theBytes, stream #) #), (# | () #) #) #)
          _ ->
            let ay = int2Word# (neWord# expected0 aw)
                by = int2Word# (neWord# expected1 bw)
                cy = int2Word# (neWord# expected2 cw)
                artifact = uncheckedShiftL# ay 2#
                     `or#` uncheckedShiftL# by 1#
                     `or#` cy
                res = 3# -# (8# -# word2Int# (clz8# artifact))
                e = case res of
                  0# -> e0 
                  1# -> e1
                  _ -> e2
             in (# s, (# (# | (# unsafeDrop# res theBytes, stream #) #), (# e | #) #) #)
    )

-- Arguments should be 8-bit words. This should be exactly
-- equivalent to calling byteUnboxed four times.
byteFourUnboxed :: forall e. e -> e -> e -> e -> Word# -> Word# -> Word# -> Word# -> ParserLevity e 'LiftedRep ()
byteFourUnboxed e0 e1 e2 e3 expected0 expected1 expected2 expected3 = ParserLevity go where
  go :: Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# e s 'LiftedRep () #)
  go m s0 = withAtLeast m 4# s0
    (getParserLevity
      (bindBoxed (byteUnboxed e0 expected0)
        (\_ -> bindBoxed (byteUnboxed e1 expected1)
          (\_ -> bindBoxed (byteUnboxed e2 expected2)
            (\_ -> byteUnboxed e3 expected3)
          )
        )
      ) 
    )
    (\theBytes@(# arr, off, _ #) stream s ->
        let aw = indexWord8Array# arr off
            bw = indexWord8Array# arr (off +# 1#)
            cw = indexWord8Array# arr (off +# 2#)
            dw = indexWord8Array# arr (off +# 3#)
            ax = eqWord# expected0 aw
            bx = eqWord# expected1 bw
            cx = eqWord# expected2 cw
            dx = eqWord# expected3 dw
         in case ax `andI#` bx `andI#` cx `andI#` dx of
          1# -> (# s, (# (# | (# unsafeDrop# 4# theBytes, stream #) #), (# | () #) #) #)
          _ ->
            let ay = int2Word# (neWord# expected0 aw)
                by = int2Word# (neWord# expected1 bw)
                cy = int2Word# (neWord# expected2 cw)
                dy = int2Word# (neWord# expected3 dw)
                artifact = uncheckedShiftL# ay 3#
                     `or#` uncheckedShiftL# by 2#
                     `or#` uncheckedShiftL# cy 1#
                     `or#` dy
                res = 4# -# (8# -# word2Int# (clz8# artifact))
                e = case res of
                  0# -> e0 
                  1# -> e1
                  2# -> e2
                  _ -> e3
             in (# s, (# (# | (# unsafeDrop# res theBytes, stream #) #), (# e | #) #) #)
    )

optionalByteUnboxed :: Word8 -> ParserLevity e 'LiftedRep Bool
optionalByteUnboxed (W8# expected) = ParserLevity go where
  go :: Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# e s 'LiftedRep Bool #)
  go m s0 = withNonEmpty m s0
    (\s -> (# s, (# (# (# #) | #), (# | False #) #) #))
    (\actual theBytes stream s -> case eqWord# expected actual of
      1# -> (# s, (# (# | (# unsafeDrop# 1# theBytes, stream #) #), (# | True #) #) #)
      _ -> (# s, (# (# | (# theBytes, stream #) #), (# | False #) #) #)
    )

optionalPlusMinusUnboxed :: ParserLevity e 'LiftedRep Bool
optionalPlusMinusUnboxed = ParserLevity go where
  go :: Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# e s 'LiftedRep Bool #)
  go m s0 = withNonEmpty m s0
    (\s -> (# s, (# (# (# #) | #), (# | True #) #) #))
    (\actual theBytes stream s -> case actual of
      43## -> (# s, (# (# | (# unsafeDrop# 1# theBytes, stream #) #), (# | True #) #) #)
      45## -> (# s, (# (# | (# unsafeDrop# 1# theBytes, stream #) #), (# | False #) #) #)
      _ -> (# s, (# (# | (# theBytes, stream #) #), (# | True #) #) #)
    )

anyUnboxed :: forall e. e -> ParserLevity e 'WordRep Word#
anyUnboxed e = ParserLevity go where
  go :: Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# e s 'WordRep Word# #)
  go m s0 = withNonEmpty m s0
    (\s -> (# s, (# (# (# #) | #), (# e | #) #) #))
    (\theByte theBytes stream s ->
      (# s, (# (# | (# unsafeDrop# 1# theBytes, stream #) #), (# | theByte #) #) #)
    )

peekUnboxed :: forall e. e -> ParserLevity e 'WordRep Word#
peekUnboxed e = ParserLevity go where
  go :: Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# e s 'WordRep Word# #)
  go m s0 = withNonEmpty m s0
    (\s -> (# s, (# (# (# #) | #), (# e | #) #) #))
    (\theByte theBytes stream s ->
      (# s, (# (# | (# theBytes, stream #) #), (# | theByte #) #) #)
    )

-- | Returns true if there is no more input and false otherwise.
-- This parser always succeeds.
isEndOfInput :: Parser e Bool
isEndOfInput = Parser (ParserLevity go) where
  go :: Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# e s 'LiftedRep Bool #)
  go m s0 = withNonEmpty m s0
    (\s -> (# s, (# (# (# #) | #), (# | True #) #) #))
    (\_ theBytes stream s -> 
      (# s, (# (# | (# theBytes, stream #) #), (# | False #) #) #)
    )

endOfInputUnboxed :: forall e. e -> ParserLevity e 'LiftedRep ()
endOfInputUnboxed e = ParserLevity go where
  go :: Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# e s 'LiftedRep () #)
  go m s0 = withNonEmpty m s0
    (\s -> (# s, (# (# (# #) | #), (# | () #) #) #))
    (\_ theBytes stream s -> 
      (# s, (# (# | (# theBytes, stream #) #), (# e | #) #) #)
    )

-- | Only succeed if there is no more input remaining.
endOfInput :: e -> Parser e ()
endOfInput e = Parser (endOfInputUnboxed e)

-- | Consume the next byte from the input.
any :: e -> Parser e Word8
any e = Parser (boxWord8Parser (anyUnboxed e))

-- | Look at the next byte without consuming it. This parser will
-- fail if there is no more input.
peek :: e -> Parser e Word8
peek e = Parser (boxWord8Parser (peekUnboxed e))

-- | Consume a byte matching the specified one.
byte :: e -> Word8 -> Parser e ()
{-# NOINLINE[2] byte #-}
byte e0 (W8# theByte) = Parser (byteUnboxed e0 theByte)

{-# RULES "byte{right,1 + 1 = 2}" [~2] forall e0 e1 a b. sequenceRight (byte e0 a) (byte e1 b) = byteTwo e0 e1 a b #-}
{-# RULES "byte{right,2 + 1 = 3}" [~2] forall e0 e1 e2 a b c. sequenceRight (byteTwo e0 e1 a b) (byte e2 c) = byteThree e0 e1 e2 a b c #-}
{-# RULES "byte{right,1 + 2 = 3}" [~2] forall e0 e1 e2 a b c. sequenceRight (byte e0 a) (byteTwo e1 e2 b c) = byteThree e0 e1 e2 a b c #-}
{-# RULES "byte{right,1 + 3 = 4}" [~2] forall e0 e1 e2 e3 a b c d. sequenceRight (byte e0 a) (byteThree e1 e2 e3 b c d) = byteFour e0 e1 e2 e3 a b c d #-}
{-# RULES "byte{right,2 + 2 = 4}" [~2] forall e0 e1 e2 e3 a b c d. sequenceRight (byteTwo e0 e1 a b) (byteTwo e2 e3 c d) = byteFour e0 e1 e2 e3 a b c d #-}
{-# RULES "byte{right,3 + 1 = 4}" [~2] forall e0 e1 e2 e3 a b c d. sequenceRight (byteThree e0 e1 e2 a b c) (byte e3 d) = byteFour e0 e1 e2 e3 a b c d #-}

{-# RULES "byte{left,1 + 1 = 2}" [~2] forall e0 e1 a b. sequenceLeft (byte e0 a) (byte e1 b) = byteTwo e0 e1 a b #-}
{-# RULES "byte{left,2 + 1 = 3}" [~2] forall e0 e1 e2 a b c. sequenceLeft (byteTwo e0 e1 a b) (byte e2 c) = byteThree e0 e1 e2 a b c #-}
{-# RULES "byte{left,1 + 2 = 3}" [~2] forall e0 e1 e2 a b c. sequenceLeft (byte e0 a) (byteTwo e1 e2 b c) = byteThree e0 e1 e2 a b c #-}

byteTwo :: e -> e -> Word8 -> Word8 -> Parser e ()
{-# NOINLINE[2] byteTwo #-}
byteTwo e0 e1 (W8# a) (W8# b) = Parser (byteTwoUnboxed e0 e1 a b)

byteThree :: e -> e -> e -> Word8 -> Word8 -> Word8 -> Parser e ()
{-# NOINLINE[2] byteThree #-}
byteThree e0 e1 e2 (W8# a) (W8# b) (W8# c) = Parser (byteThreeUnboxed e0 e1 e2 a b c)

byteFour :: e -> e -> e -> e -> Word8 -> Word8 -> Word8 -> Word8 -> Parser e ()
{-# NOINLINE[2] byteFour #-}
byteFour e0 e1 e2 e3 (W8# a) (W8# b) (W8# c) (W8# d) = Parser (byteFourUnboxed e0 e1 e2 e3 a b c d)

-- | Consume a byte matching the specified one. If the next byte
-- in the input does not match the given byte, it is not consumed.
-- This parser always succeeds even if there is no input. It returns
-- True if the byte was present (and consumed) and False is the byte
-- was not present.
optionalByte :: Word8 -> Parser e Bool
optionalByte theByte = Parser (optionalByteUnboxed theByte)

-- | Consume a byte that is either a plus sign or a minus sign.
-- If neither of those two bytes follow or if the stream is empty,
-- consume nothing. This always succeeds. If a plus sign was consumed
-- or if nothing was consumed, returns True. If a minus sign was
-- consumed, returns False.
optionalPlusMinus :: Parser e Bool
optionalPlusMinus = Parser optionalPlusMinusUnboxed

bytes :: e -> Bytes -> Parser e ()
bytes e b = Parser (bytesUnboxed e b)

bytesUnboxed :: forall e. e -> Bytes -> ParserLevity e 'LiftedRep ()
bytesUnboxed e (Bytes parr poff plen) = ParserLevity (go (unboxInt poff)) where
  !(I# pend) = poff + plen
  go :: Int# -> Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# e s 'LiftedRep () #)
  go !ix (# (# #) | #) s0 = case ix ==# pend of
    1# -> (# s0, (# (# (# #) | #), (# | () #) #) #)
    _ -> (# s0, (# (# (# #) | #), (# e | #) #) #)
  go !ix (# | leftovers@(# bytes0@(# arr, off, len #), !stream0@(ByteStream streamFunc) #) #) s0 = 
    case BAW.stripPrefixResumable (I# ix) (I# off) (I# (pend -# ix)) (I# len) parr (ByteArray arr) of
      (# (# #) | | #) -> (# s0, (# (# | leftovers #), (# e | #) #) #)
      (# | (# #) | #) -> (# s0, (# (# | (# unsafeDrop# (pend -# ix) bytes0, stream0 #) #), (# | () #) #) #)
      (# | | (# #) #) -> case streamFunc s0 of
        (# s1, r #) -> go (ix +# len) r s1

-- replicateUntilMember :: forall e c a. ByteSet -> Parser e c a -> Parser (Array a)
-- replicateUntilMember separators b p = go []
--   where
--   go :: [a] -> Parser
--   go !xs = 
  
-- | Repeat the parser a specified number of times. The implementation
--   is tail recursive and avoids building up a list as an intermediate
--   data structure. Instead, it writes to an array directly. 
replicate :: forall e a. Int -> Parser e a -> Parser e (Array a)
replicate (I# total) (Parser (ParserLevity f)) =
  Parser (ParserLevity (\m s0 -> case newArray# total (die "replicate") s0 of
    (# s1, xs0 #) -> go 0# xs0 m s1
  ))
  where
  go :: Int# -> MutableArray# s a -> Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# e s 'LiftedRep (Array a) #)
  go !n !xs !m0 !s0 = case n <# total of
    1# -> case f m0 s0 of
      (# s1, (# m1, v #) #) -> case v of
        (# err | #) -> (# s1, (# m1, (# err | #) #) #)
        (# | x #) -> case writeArray# xs n x s1 of
          s2 -> go (n +# 1# ) xs m1 s2
    _ -> case unsafeFreezeArray# xs s0 of
      (# s1, xsFrozen #) -> (# s1, (# m0, (# | Array xsFrozen #) #) #)

-- | This resets the context on every iteration.
replicateIndex# :: forall e a. (Int# -> e) -> Int -> (e -> Parser e a) -> Parser e (Array a)
replicateIndex# buildContext (I# total) f =
  Parser (ParserLevity (\m s0 -> case newArray# total (die "replicate") s0 of
    (# s1, xs0 #) -> go 0# xs0 m s1
  ))
  where
  go :: Int# -> MutableArray# s a -> Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# e s 'LiftedRep (Array a) #)
  go !n !xs !m0 !s0 = case n <# total of
    1# -> case getParserLevity (getParser (f (buildContext n))) m0 s0 of
      (# s1, (# m1, v #) #) -> case v of
        (# err | #) -> (# s1, (# m1, (# err | #) #) #)
        (# | x #) -> case writeArray# xs n x s1 of
          s2 -> go (n +# 1# ) xs m1 s2
    _ -> case unsafeFreezeArray# xs s0 of
      (# s1, xsFrozen #) -> (# s1, (# m0, (# | Array xsFrozen #) #) #)
        
-- | Repeatly run the folding parser followed by the separator parser
-- until the end of the input is reached.
foldIntersperseParserUntilEnd :: 
     Parser e b -- ^ separator, result is discarded
  -> a -- ^ initial accumulator
  -> (a -> Parser e a) -- ^ parser that takes previous accumulator
  -> Parser e a
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
     e -- ^ unneeded, fix this 
  -> Word8 -- ^ separator
  -> a -- ^ initial accumulator
  -> (a -> Parser e a) -- ^ parser that takes previous accumulator
  -> Parser e a
foldIntersperseByte e !sep !a0 p = isEndOfInput >>= \case
  True -> return a0
  False -> do
    a1 <- p a0
    let go !a = isEndOfInput >>= \case
          True -> return a
          False -> do
            b <- peek e
            if b == sep
              then do
                _ <- any e
                p a >>= go
              else return a
    go a1

-- replicateIntersperseUntilEnd :: forall e c a b. Parser e c b -> Parser e c a -> Parser e c (Array a)
-- replicateIntersperseUntilEnd = _

-- replicateUntilMember :: forall e c a. ByteSet -> Parser e c a -> Parser (Array a)
-- replicateUntilMember !set (Parser (ParserLevity f)) = Parser (ParserLevity (go 0# [])) where


-- | Run the parser repeatedly until the end of the stream is
-- encountered.
replicateUntilEnd :: forall e a. Parser e a -> Parser e (Array a)
replicateUntilEnd (Parser (ParserLevity f)) = Parser (ParserLevity (go 0# [])) where
  go :: Int# -> [a] -> Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# e s 'LiftedRep (Array a) #)
  go !n !xs !m !s0 = withNonEmpty m s0
    (\s -> 
      let theArray :: Array a
          !theArray = reverseArrayFromListN "replicateUntilEnd" (I# n) xs
       in (# s, (# (# (# #) | #), (# | theArray #) #) #)
    )
    (\_ theBytes stream s1 -> case f (# | (# theBytes, stream #) #) s1 of
      (# s2, (# leftovers, res #) #) -> case res of
        (# err | #) -> (# s2, (# leftovers, (# err | #) #) #)
        (# | !x #) -> go (n +# 1# ) (x : xs) leftovers s2
    )

-- | Replicate the parser as long as we encounter a byte belonging to 
-- the given byte set after each run of the parser. The byte set can be
-- understood as all the bytes are considered separators. If the stream
-- ends where a separator is expected, this is considered a successful parse.
replicateIntersperseMember :: forall e a. ByteSet -> Parser e a -> Parser e (Array a)
-- THIS IS BROKEN. FIX IT.
replicateIntersperseMember !set (Parser (ParserLevity f)) = Parser (ParserLevity (go 0# [])) where
  go :: Int# -> [a] -> Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# e s 'LiftedRep (Array a) #)
  go !n !xs !m !s0 = withNonEmpty m s0
    (\s1 ->
        let theArray :: Array a
            !theArray = reverseArrayFromListN "replicateIntersperseMember" (I# n) xs
         in (# s1, (# (# (# #) | #), (# | theArray #) #) #)
    )
    (\theByte theBytes stream s1 -> case ByteSet.member (W8# theByte) set of
      True -> case f (# | (# theBytes, stream #) #) s1 of
        (# s2, (# leftovers, res #) #) -> case res of
          (# err | #) -> (# s2, (# leftovers, (# err | #) #) #)
          (# | !x #) -> go (n +# 1# ) (x : xs) leftovers s2
      False ->
        let theArray :: Array a
            !theArray = reverseArrayFromListN "replicateIntersperseMember" (I# n) xs
         in (# s1, (# (# | (# theBytes, stream #) #), (# | theArray #) #) #)
    )

replicateIntersperseBytePrim :: forall e a. Prim a
  => Word8 -> Parser e a -> Parser e (PrimArray a)
replicateIntersperseBytePrim !(W8# sepByte) (Parser (ParserLevity f)) =
  Parser (ParserLevity (onceThenReplicatePrim (ParserLevity f) go))
  where
  go :: Int# -> Int# -> MutableByteArray# s -> Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# e s 'LiftedRep (PrimArray a) #)
  go !ix !sz !marr !m !s0 = withNonEmpty m s0
    (\s1 -> case shrinkMutableByteArray# marr (ix *# PM.sizeOf# (undefined :: a)) s1 of
      s2 -> case unsafeFreezeByteArray# marr s2 of
        (# s3, arr #) -> (# s3, (# (# (# #) | #), (# | PrimArray arr #) #) #)
    )
    (\theByte theBytes stream s1 -> case eqWord# theByte sepByte of
      1# -> case f (# | (# unsafeDrop# 1# theBytes, stream #) #) s1 of
        (# s2, (# leftovers, res #) #) -> case res of
          (# err | #) -> (# s2, (# leftovers, (# err | #) #) #)
          (# | !x #) -> case ix <# sz of
            1# -> case PM.writeByteArray# marr ix x s2 of
              s3 -> go (ix +# 1# ) sz marr leftovers s3
            _ -> case newByteArray# (sz *# 2# *# PM.sizeOf# (undefined :: a)) s2 of
              (# s3, marrNew #) -> case copyMutableByteArray# marr 0# marrNew 0# (sz *# PM.sizeOf# (undefined :: a)) s3 of
                s4 -> case PM.writeByteArray# marrNew ix x s4 of
                  s5 -> go (ix +# 1#) (sz *# 2#) marrNew leftovers s5
      _ -> case shrinkMutableByteArray# marr (ix *# PM.sizeOf# (undefined :: a)) s1 of
        s2 -> case unsafeFreezeByteArray# marr s2 of
          (# s3, arr #) -> (# s3, (# (# | (# theBytes, stream #) #), (# | PrimArray arr #) #) #)
    )

-- | Replicate the parser as long as we encounter the specified byte
-- after each run of the parser. If the stream ends where the separator
-- is expected, this is considered a successful parse.
replicateIntersperseByte :: forall e a. Word8 -> Parser e a -> Parser e (Array a)
replicateIntersperseByte !(W8# sepByte) (Parser (ParserLevity f)) =
  Parser (ParserLevity (onceThenReplicate (ParserLevity f) go))
  where
  go :: Int# -> [a] -> Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# e s 'LiftedRep (Array a) #)
  go !n !xs !m !s0 = withNonEmpty m s0
    (\s1 ->
        let theArray :: Array a
            !theArray = reverseArrayFromListN "replicateIntersperseByte" (I# n) xs
         in (# s1, (# (# (# #) | #), (# | theArray #) #) #)
    )
    (\theByte theBytes stream s1 -> case eqWord# theByte sepByte of
      1# -> case f (# | (# unsafeDrop# 1# theBytes, stream #) #) s1 of
        (# s2, (# leftovers, res #) #) -> case res of
          (# err | #) -> (# s2, (# leftovers, (# err | #) #) #)
          (# | !x #) -> go (n +# 1# ) (x : xs) leftovers s2
      _ -> 
        let theArray :: Array a
            !theArray = reverseArrayFromListN "replicateIntersperseByte" (I# n) xs
         in (# s1, (# (# | (# theBytes, stream #) #), (# | theArray #) #) #)
    )

-- | Replicate the parser as long as we encounter the specified byte
-- after each run of the parser. If the stream ends where the separator
-- is expected, this is considered a successful parse.
replicateIntersperseByteIndex# :: forall e c a.
     c
  -> (Int# -> c -> c)
  -> Word8
  -> (c -> Parser e a)
  -> Parser e (Array a)
replicateIntersperseByteIndex# e0 applyIndex !(W8# sepByte) f =
  Parser (ParserLevity (onceThenReplicateIndex# e0 applyIndex (getParser . f) go))
  where
  go :: Int# -> Int# -> [a] -> Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# e s 'LiftedRep (Array a) #)
  go !ix !n !xs !m !s0 = withNonEmpty m s0
    (\s1 ->
        let theArray :: Array a
            !theArray = reverseArrayFromListN "replicateIntersperseByteIndex#" (I# n) xs
         in (# s1, (# (# (# #) | #), (# | theArray #) #) #)
    )
    (\theByte theBytes stream s1 -> case eqWord# theByte sepByte of
      1# -> case getParserLevity (getParser (f (applyIndex ix e0))) (# | (# unsafeDrop# 1# theBytes, stream #) #) s1 of
        (# s2, (# leftovers, res #) #) -> case res of
          (# err | #) -> (# s2, (# leftovers, (# err | #) #) #)
          (# | !x #) -> go (ix +# 1#) (n +# 1# ) (x : xs) leftovers s2
      _ -> 
        let theArray :: Array a
            !theArray = reverseArrayFromListN "replicateIntersperseByteIndex#" (I# n) xs
         in (# s1, (# (# | (# theBytes, stream #) #), (# | theArray #) #) #)
    )

-- This succeeds if the input is empty. Also, this resets the context
-- after successfully parsing the first element.
onceThenReplicateIndex# ::
     c
  -> (Int# -> c -> c)
  -> (c -> ParserLevity e 'LiftedRep a)
  -> (Int# -> Int# -> [a] -> Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# e s 'LiftedRep (Array a) #))
  -> (Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# e s 'LiftedRep (Array a) #))
onceThenReplicateIndex# e0 applyIndex f go m s0 = withNonEmpty m s0
  (\s1 -> (# s1, (# (# (# #) | #), (# | mempty #) #) #)
  )
  (\_ theBytes stream s1 -> case getParserLevity (f (applyIndex 0# e0)) (# | (# theBytes, stream #) #) s1 of
    (# s2, (# leftovers, res #) #) -> case res of
      (# err | #) -> (# s2, (# leftovers, (# err | #) #) #)
      (# | !x #) -> go 1# 1# [x] leftovers s2
  )

-- this succeeds if the input is empty 
onceThenReplicate ::
     ParserLevity e 'LiftedRep a
  -> (Int# -> [a] -> Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# e s 'LiftedRep (Array a) #))
  -> (Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# e s 'LiftedRep (Array a) #))
onceThenReplicate (ParserLevity f) go m s0 = withNonEmpty m s0
  (\s1 -> (# s1, (# (# (# #) | #), (# | mempty #) #) #)
  )
  (\_ theBytes stream s1 -> case f (# | (# theBytes, stream #) #) s1 of
    (# s2, (# leftovers, res #) #) -> case res of
      (# err | #) -> (# s2, (# leftovers, (# err | #) #) #)
      (# | !x #) -> go 1# [x] leftovers s2
  )

onceThenReplicatePrim :: forall e s a. Prim a
  => ParserLevity e 'LiftedRep a
  -> (Int# -> Int# -> MutableByteArray# s -> Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# e s 'LiftedRep (PrimArray a) #))
  -> (Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# e s 'LiftedRep (PrimArray a) #))
onceThenReplicatePrim (ParserLevity f) go m s0 = withNonEmpty m s0
  (\s1 -> (# s1, (# (# (# #) | #), (# | mempty #) #) #)
  )
  (\_ theBytes stream s1 -> case f (# | (# theBytes, stream #) #) s1 of
    (# s2, (# leftovers, res #) #) -> case res of
      (# err | #) -> (# s2, (# leftovers, (# err | #) #) #)
      (# | !x #) -> let sz = chooseMinElementSize (PM.sizeOf# (undefined :: a)) in
        case newByteArray# (sz *# PM.sizeOf# (undefined :: a)) s2 of
          (# s3, marr #) -> case PM.writeByteArray# marr 0# x s3 of
            s4 -> go 1# sz marr leftovers s4
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
trie :: e -> Trie (Parser e a) -> Parser e a
trie err = triePure err >=> id

triePure :: e -> Trie a -> Parser e a
triePure err t = do
  e <- Trie.lookupM (any err) (bytes err . B.fromByteArray) (return True) return t
  case e of
    Left _ -> failure err
    Right x -> return x

-- -- | Variant of 'trie' whose parsers accept an environment. This is helpful
-- -- in situations where the user needs to ensure that the 'Trie' used by
-- -- the parser is a CAF.
-- trieReader :: Trie (r -> Parser e c a) -> r -> Parser e c a
-- trieReader (Trie.Trie node) = go node where
--   go :: forall e c d b t. Trie.Node d (t -> Parser e c b) -> t -> Parser e c b
--   go Trie.NodeEmpty _ = failure
--   go (Trie.NodeValueNil p) r = p r
--   go (Trie.NodeRun (Trie.Run arr n)) r = do
--     bytes (B.fromByteArray arr)
--     go n r
--   go (Trie.NodeBranch arr) r = do
--     b <- any
--     go (PM.indexArray arr (word8ToInt b)) r
--   go (Trie.NodeValueRun p _) r = p r
--   go (Trie.NodeValueBranch p _) r = p r
-- 
-- -- | Variant of 'trie' whose parsers modify state and accept an environment.
-- trieReaderState :: Trie (r -> s -> Parser e c (s, a)) -> r -> s -> Parser e c (s,a)
-- trieReaderState (Trie.Trie node) = go node where
--   go :: forall e c d b r' s'. Trie.Node d (r' -> s' -> Parser e c (s',b)) -> r' -> s' -> Parser e c (s',b)
--   go Trie.NodeEmpty _ _ = failure
--   go (Trie.NodeValueNil p) r s = p r s
--   go (Trie.NodeRun (Trie.Run arr n)) r s = do
--     bytes (B.fromByteArray arr)
--     go n r s
--   go (Trie.NodeBranch arr) r s = do
--     b <- any
--     go (PM.indexArray arr (word8ToInt b)) r s
--   go (Trie.NodeValueRun p _) r s = p r s
--   go (Trie.NodeValueBranch p _) r s = p r s
-- 
-- -- | Variant of 'trie' whose parsers modify state and accept an environment.
-- -- This variant lacks a result type. It only modifies the state.
-- trieReaderState_ :: Trie (r -> s -> Parser e c s) -> r -> s -> Parser e c s
-- trieReaderState_ (Trie.Trie node) = go node where
--   go :: forall e c d r' s'. Trie.Node d (r' -> s' -> Parser e c s') -> r' -> s' -> Parser e c s'
--   go Trie.NodeEmpty _ _ = failure
--   go (Trie.NodeValueNil p) r s = p r s
--   go (Trie.NodeRun (Trie.Run arr n)) r s = do
--     bytes (B.fromByteArray arr)
--     go n r s
--   go (Trie.NodeBranch arr) r s = do
--     b <- any
--     go (PM.indexArray arr (word8ToInt b)) r s
--   go (Trie.NodeValueRun p _) r s = p r s
--   go (Trie.NodeValueBranch p _) r s = p r s


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

skipSpace :: Parser e ()
{-# NOINLINE[2] skipSpace #-}
skipSpace = Parser skipSpaceUnboxed
{-# RULES "skipSpace-skipSpace{right}" [~2] sequenceRight skipSpace skipSpace = skipSpace #-}
{-# RULES "skipSpace-skipSpace{left}" [~2] sequenceLeft skipSpace skipSpace = skipSpace #-}

-- | Parse an ascii-encoded number in decimal notation. This
-- will succeed even for numbers that are too big to fit into
-- a machine word.
decimalWord :: e -> Parser e Word
{-# NOINLINE[2] decimalWord #-}
decimalWord e = Parser (boxWordParser (decimalWordUnboxed e))
-- TODO: write skipDigits1 and enable these rules
-- {-# RULES "decimalWord-unused{right}" [~2] forall e a. sequenceRight (decimalWord e) a = sequenceRight (skipDigits1 e) a #-}
-- {-# RULES "decimalWord-unused{left}" [~2] forall e a. sequenceLeft a (decimalWord e) = sequenceLeft a (skipDigits1 e) #-}

decimalInt :: e -> Parser e Int
decimalInt e = do
  hasMinus <- optionalByte (charToWord8 '-')
  w <- decimalWord e
  let i = wordToInt w
  return (if hasMinus then negate i else i)

-- | Parse a decimal word, attaching the provided value (should be
-- between 0 and 9 inclusive) to the front of it.
decimalWordStarting :: Word -> Parser e Word
decimalWordStarting (W# w0) = Parser (boxWordParser (decimalContinue w0))

decimalDigitWord :: e -> Parser e Word
decimalDigitWord e = Parser (boxWordParser (decimalDigit e))

optionalDecimalDigitWord :: Parser e (Maybe Word)
optionalDecimalDigitWord = Parser (boxMaybeWordParser optionalDecimalDigit)

-- | Run a stateful parser.
statefully :: (forall s. StatefulParser e s a) -> Parser e a
statefully x = Parser (ParserLevity (case x of {StatefulParser f -> f}))

-- | Lift a 'ST' action into a stateful parser.
mutation :: ST s a -> StatefulParser e s a
mutation (ST f) = StatefulParser $ \leftovers0 s0 ->
  case f s0 of
    (# s1, a #) -> (# s1, (# leftovers0, (# | a #) #) #)

-- | Lift a pure parser into a stateful parser.
consumption :: Parser e a -> StatefulParser e s a
consumption (Parser (ParserLevity f)) = StatefulParser f

-- TODO: improve this
mapParser :: (a -> b) -> Parser e a -> Parser e b
mapParser f p = bindLifted p (pureParser . f)

pureParser :: a -> Parser e a
pureParser a = Parser $ ParserLevity $ \leftovers0 s0 ->
  (# s0, (# leftovers0, (# | a #) #) #)

pureWord :: Word# -> ParserLevity e 'WordRep Word#
pureWord a = ParserLevity $ \leftovers0 s0 ->
  (# s0, (# leftovers0, (# | a #) #) #)

-- TODO: improve this
mapStatefulParser :: (a -> b) -> StatefulParser e s a -> StatefulParser e s b
mapStatefulParser f p = bindStateful p (pureStatefulParser . f)

pureStatefulParser :: a -> StatefulParser e s a
pureStatefulParser a = StatefulParser $ \leftovers0 s0 ->
  (# s0, (# leftovers0, (# | a #) #) #)

bindStateful :: StatefulParser e s a -> (a -> StatefulParser e s b) -> StatefulParser e s b
bindStateful (StatefulParser f) g = StatefulParser $ \leftovers0 s0 -> case f leftovers0 s0 of
  (# s1, (# leftovers1, val #) #) -> case val of
    (# err | #) -> (# s1, (# leftovers1, (# err | #) #) #)
    (# | x #) -> case g x of
      StatefulParser k -> k leftovers1 s1

bindLifted :: Parser e a -> (a -> Parser e b) -> Parser e b
bindLifted (Parser (ParserLevity f)) g = Parser $ ParserLevity $ \leftovers0 s0 -> case f leftovers0 s0 of
  (# s1, (# leftovers1, val #) #) -> case val of
    (# err | #) -> (# s1, (# leftovers1, (# err | #) #) #)
    (# | x #) -> case g x of
      Parser (ParserLevity k) -> k leftovers1 s1

bindBoxed :: ParserLevity e 'LiftedRep a -> (a -> ParserLevity e 'LiftedRep b) -> ParserLevity e 'LiftedRep b
bindBoxed (ParserLevity f) g = ParserLevity $ \leftovers0 s0 -> case f leftovers0 s0 of
  (# s1, (# leftovers1, val #) #) -> case val of
    (# err | #) -> (# s1, (# leftovers1, (# err | #) #) #)
    (# | x #) -> case g x of
      ParserLevity k -> k leftovers1 s1


bindWord :: ParserLevity e 'WordRep Word# -> (Word# -> ParserLevity e 'WordRep Word#) -> ParserLevity e 'WordRep Word#
bindWord (ParserLevity f) g = ParserLevity $ \leftovers0 s0 -> case f leftovers0 s0 of
  (# s1, (# leftovers1, val #) #) -> case val of
    (# err | #) -> (# s1, (# leftovers1, (# err | #) #) #)
    (# | x #) -> case g x of
      ParserLevity k -> k leftovers1 s1

boxWord8Parser :: ParserLevity e 'WordRep Word# -> ParserLevity e 'LiftedRep Word8
boxWord8Parser p = ParserLevity $ \leftovers0 s0 ->
  case getParserLevity p leftovers0 s0 of
    (# s1, (# leftovers1, val #) #) -> case val of
      (# err | #) -> (# s1, (# leftovers1, (# err | #) #) #)
      (# | x #) -> (# s1, (# leftovers1, (# | W8# x #) #) #)

boxWord32Parser :: ParserLevity e 'WordRep Word# -> ParserLevity e 'LiftedRep Word32
boxWord32Parser p = ParserLevity $ \leftovers0 s0 ->
  case getParserLevity p leftovers0 s0 of
    (# s1, (# leftovers1, val #) #) -> case val of
      (# err | #) -> (# s1, (# leftovers1, (# err | #) #) #)
      (# | x #) -> (# s1, (# leftovers1, (# | W32# x #) #) #)

boxWordParser :: ParserLevity e 'WordRep Word# -> ParserLevity e 'LiftedRep Word
boxWordParser p = ParserLevity $ \leftovers0 s0 ->
  case getParserLevity p leftovers0 s0 of
    (# s1, (# leftovers1, val #) #) -> case val of
      (# err | #) -> (# s1, (# leftovers1, (# err | #) #) #)
      (# | x #) -> (# s1, (# leftovers1, (# | W# x #) #) #)

boxMaybeWordParser ::
     ParserLevity e ('SumRep '[ 'TupleRep '[], 'WordRep]) (Maybe# Word#)
  -> ParserLevity e 'LiftedRep (Maybe Word)
boxMaybeWordParser p = ParserLevity $ \leftovers0 s0 ->
  case getParserLevity p leftovers0 s0 of
    (# s1, (# leftovers1, val #) #) -> case val of
      (# err | #) -> (# s1, (# leftovers1, (# err | #) #) #)
      (# | m #) -> case m of
         (# (# #) | #) -> (# s1, (# leftovers1, (# | Nothing #) #) #)
         (# | x #) -> (# s1, (# leftovers1, (# | Just (W# x) #) #) #)

boxBytesWord8Parser ::
     ParserLevity e ('TupleRep '[BytesRep, 'WordRep]) (# Bytes#, Word# #)
  -> ParserLevity e 'LiftedRep (Bytes,Word8)
boxBytesWord8Parser p = ParserLevity $ \leftovers0 s0 ->
  case getParserLevity p leftovers0 s0 of
    (# s1, (# leftovers1, val #) #) -> case val of
      (# err | #) -> (# s1, (# leftovers1, (# err | #) #) #)
      (# | (# theBytes, theWord #) #) -> (# s1, (# leftovers1, (# | (boxBytes theBytes, W8# theWord) #) #) #)

boxBytesParser ::
     ParserLevity e BytesRep Bytes#
  -> ParserLevity e 'LiftedRep Bytes
boxBytesParser p = ParserLevity $ \leftovers0 s0 ->
  case getParserLevity p leftovers0 s0 of
    (# s1, (# leftovers1, val #) #) -> case val of
      (# err | #) -> (# s1, (# leftovers1, (# err | #) #) #)
      (# | theBytes #) -> (# s1, (# leftovers1, (# | boxBytes theBytes #) #) #)

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
endOfLine :: e -> Parser e ()
-- In the event of a failure in the carriage-return case, this
-- currently consumes a character from the leftovers that
-- should be left alone. Could be hand-written to be faster and
-- more correct.
endOfLine e = do
  w <- any e
  case w of
    10 -> return ()
    13 -> byte e 10
    _ -> failure e

failure :: e -> Parser e a
failure e = Parser (ParserLevity (\m s -> (# s, (# m, (# e | #) #) #)))

charToWord8 :: Char -> Word8
charToWord8 = fromIntegral . ord

charUtf8 :: e -> Parser e Char
charUtf8 e = do
  a <- any e
  if | oneByteChar a -> return (word8ToChar a)
     | twoByteChar a -> do
         b <- any e
         when (not (followingByte b)) (failure e)
         return (charFromTwoBytes a b)
     | threeByteChar a -> do
         b <- any e
         when (not (followingByte b)) (failure e)
         c <- any e
         when (not (followingByte c)) (failure e)
         let w = codepointFromThreeBytes a b c
         return (if surrogate w then '\xFFFD' else unsafeWordToChar w)
     | fourByteChar a -> do
         b <- any e
         when (not (followingByte b)) (failure e)
         c <- any e
         when (not (followingByte c)) (failure e)
         d <- any e
         when (not (followingByte d)) (failure e)
         return (charFromFourBytes a b c d)
     | otherwise -> failure e

word8ToChar :: Word8 -> Char
word8ToChar (W8# w) = C# (chr# (word2Int# w))

unsafeWordToChar :: Word -> Char
unsafeWordToChar (W# w) = C# (chr# (word2Int# w))

followingByte :: Word8 -> Bool
followingByte !w = xor w 0b01000000 .&. 0b11000000 == 0b11000000

oneByteChar :: Word8 -> Bool
oneByteChar !w = w .&. 0b10000000 == 0

twoByteChar :: Word8 -> Bool
twoByteChar !w = w .&. 0b11100000 == 0b11000000

threeByteChar :: Word8 -> Bool
threeByteChar !w = w .&. 0b11110000 == 0b11100000

fourByteChar :: Word8 -> Bool
fourByteChar !w = w .&. 0b11111000 == 0b11110000

charFromTwoBytes :: Word8 -> Word8 -> Char
charFromTwoBytes w1 w2 = unsafeWordToChar $
  unsafeShiftL (word8ToWord w1 .&. 0b00011111) 6 .|. 
  (word8ToWord w2 .&. 0b00111111)

codepointFromThreeBytes :: Word8 -> Word8 -> Word8 -> Word
codepointFromThreeBytes w1 w2 w3 = 
  unsafeShiftL (word8ToWord w1 .&. 0b00001111) 12 .|. 
  unsafeShiftL (word8ToWord w2 .&. 0b00111111) 6 .|. 
  (word8ToWord w3 .&. 0b00111111)

charFromFourBytes :: Word8 -> Word8 -> Word8 -> Word8 -> Char
charFromFourBytes w1 w2 w3 w4 = unsafeWordToChar $
  unsafeShiftL (word8ToWord w1 .&. 0b00000111) 18 .|. 
  unsafeShiftL (word8ToWord w2 .&. 0b00111111) 12 .|. 
  unsafeShiftL (word8ToWord w3 .&. 0b00111111) 6 .|. 
  (word8ToWord w4 .&. 0b00111111)


surrogate :: Word -> Bool
surrogate codepoint = codepoint >= 0xD800 && codepoint < 0xE000

-- charUtf8# :: ParserLevity 'CharRep e c Char#
-- charUtf8# =

-- jsonString :: Parser e c Text
-- jsonString = Parser $ ParserLevity $ \c0 leftovers0 s0
