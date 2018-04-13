{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}

-- {-# OPTIONS_GHC -O2 #-}

module MinimalTrieParser
  ( replicateParse
  ) where

import Data.Semigroup (Semigroup)
import Data.Word (Word8)
import Data.Map (Map)
import Data.Bifunctor (second)
import Data.Primitive (ByteArray(..),indexByteArray,unsafeFreezeByteArray,
  newByteArray,sizeofByteArray,writeByteArray)
import GHC.Word (Word8(W8#))
import GHC.Exts
import GHC.ST (ST(ST))
import Control.Monad.ST (runST)
import qualified Control.Monad
import qualified Data.Char
import qualified GHC.OldList as L
import qualified Data.Semigroup as SG
import qualified Data.Map.Strict as M

replicateParse :: Int -> Word
replicateParse n = replicateFunction n step 1

replicateFunction :: Int -> (a -> a) -> a -> a
replicateFunction !n f !a = if n > 0
  then replicateFunction (n - 1) f (f a)
  else a

step :: Word -> Word
step !w = runST $ do
  Result _ r <- executeParser (streamFromBytes (s2b "STRING: 120!")) myParser
  return (maybe 0 (+w) r)

myParser :: Parser Word
myParser = trieToParser $ trieFromList
  [ ("STRING: ", do
      w <- decimalWordParser
      byteParser (c2w '!')
      return (sum (replicate (fromIntegral w) (1 :: Word)))
    )
  ]

newtype Parser a = Parser (ParserLevity 'LiftedRep a)
newtype ParserLevity (r :: RuntimeRep) (a :: TYPE r) = ParserLevity
  { getParserLevity :: forall s.
       Maybe# (Leftovers# s)
    -> State# s
    -> (# State# s, Result# s r a #)
  }
type Result# s (r :: RuntimeRep) (a :: TYPE r) =
  (# Maybe# (Leftovers# s), Maybe# a #)
data Result s a = Result
  { resultLeftovers :: !(Maybe (Leftovers s))
  , resultValue :: !(Maybe a)
  }

data Bytes = Bytes
  {-# UNPACK #-} !ByteArray -- payload
  {-# UNPACK #-} !Int -- offset
  {-# UNPACK #-} !Int -- length
type Bytes# = (# ByteArray#, Int#, Int# #)
type Leftovers# s = (# Bytes# , ByteStream s #)
type Maybe# (a :: TYPE r) = (# (# #) | a #)
newtype ByteStream s = ByteStream
  (State# s -> (# State# s, (# (# #) | (# Bytes# , ByteStream s #) #) #) )
data Leftovers s = Leftovers
  { leftoversChunk :: {-# UNPACK #-} !Bytes
    -- ^ The last chunk pulled from the stream
  , leftoversStream :: ByteStream s
    -- ^ The remaining stream
  }

emptyStream :: ByteStream s
emptyStream = ByteStream (\s -> (# s, (# (# #) | #) #) )

streamFromBytes :: Bytes -> ByteStream s
streamFromBytes b = ByteStream
  (\s0 -> (# s0, (# | (# unboxBytes b, emptyStream #) #) #))

unboxBytes :: Bytes -> Bytes#
unboxBytes (Bytes (ByteArray a) (I# b) (I# c)) = (# a,b,c #)

emptyByteArray :: ByteArray
emptyByteArray = runST (newByteArray 0 >>= unsafeFreezeByteArray)

executeParser :: ByteStream s -> Parser a -> ST s (Result s a)
executeParser stream (Parser (ParserLevity f)) = ST $ \s0 ->
  case f (# | (# (# unboxByteArray emptyByteArray, 0#, 0# #), stream #) #) s0 of
    (# s1, r #) -> (# s1, boxResult r #)

boxResult :: Result# s 'LiftedRep a -> Result s a
boxResult (# leftovers, val #) = case val of
  (# (# #) | #) -> Result (boxLeftovers leftovers) Nothing
  (# | a #) -> Result (boxLeftovers leftovers) (Just a)

unboxByteArray :: ByteArray -> ByteArray#
unboxByteArray (ByteArray arr) = arr

-- | Consume a decimal number.
{-# NOINLINE decimalWordParser #-}
decimalWordParser :: Parser Word
decimalWordParser = Parser (boxWordParser decimalWordUnboxed)

-- | Consume a single-digit decimal number.
decimalDigitWordParser :: Parser Word
decimalDigitWordParser = Parser (boxWordParser decimalDigit)

-- | Consume a byte matching the specified one.
byteParser :: Word8 -> Parser ()
byteParser theByte = Parser (byteUnboxed theByte)

-- | Consume the next byte from the input.
anyParser :: Parser Word8
anyParser = Parser (boxWord8Parser anyUnboxed)

-- | Always fails.
failureParser :: Parser a
failureParser = Parser (ParserLevity (\m s -> (# s, (# m, (# (# #) | #) #) #)))

decimalWordUnboxed :: ParserLevity 'WordRep Word#
decimalWordUnboxed = bindWord decimalDigit decimalContinue

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
                  then go (plusWord# (timesWord# w0 10##) (unboxWord w)) (# unsafeDrop# 1# bytes1, stream1 #) s1
                  else (# s1, (# (# | (# bytes1, stream1 #) #), (# | w0 #) #) #)

boxWordParser :: ParserLevity 'WordRep Word# -> ParserLevity 'LiftedRep Word
boxWordParser p = ParserLevity $ \leftovers0 s0 ->
  case getParserLevity p leftovers0 s0 of
    (# s1, (# leftovers1, val #) #) -> case val of
      (# (# #) | #) -> (# s1, (# leftovers1, (# (# #) | #) #) #)
      (# | x #) -> (# s1, (# leftovers1, (# | W# x #) #) #)

byteUnboxed :: Word8 -> ParserLevity 'LiftedRep ()
byteUnboxed (W8# expected) = ParserLevity go where
  go :: Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# s 'LiftedRep () #)
  go m s0 = withNonEmpty m s0
    (\s -> (# s, (# (# (# #) | #), (# (# #) | #) #) #))
    (\actual theBytes stream s -> case eqWord# expected actual of
      1# -> (# s, (# (# | (# unsafeDrop# 1# theBytes, stream #) #), (# | () #) #) #)
      _ -> (# s, (# (# | (# theBytes, stream #) #), (# (# #) | #) #) #)
    )

anyUnboxed :: ParserLevity 'WordRep Word#
anyUnboxed = ParserLevity go where
  go :: Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# s 'WordRep Word# #)
  go m s0 = withNonEmpty m s0
    (\s -> (# s, (# (# (# #) | #), (# (# #) | #) #) #))
    (\theByte theBytes stream s ->
      (# s, (# (# | (# unsafeDrop# 1# theBytes, stream #) #), (# | theByte #) #) #)
    )

boxWord8Parser :: ParserLevity 'WordRep Word# -> ParserLevity 'LiftedRep Word8
boxWord8Parser p = ParserLevity $ \leftovers0 s0 ->
  case getParserLevity p leftovers0 s0 of
    (# s1, (# leftovers1, val #) #) -> case val of
      (# (# #) | #) -> (# s1, (# leftovers1, (# (# #) | #) #) #)
      (# | x #) -> (# s1, (# leftovers1, (# | W8# x #) #) #)


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
                  then (# s1, (# (# | (# unsafeDrop# 1# bytes1, stream1 #) #), (# | unboxWord w #) #) #)
                  else (# s1, (# (# | (# bytes1, stream1 #) #), (# (# #) | #) #) #)

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


unboxWord :: Word -> Word#
unboxWord (W# i) = i

word8ToWord :: Word8 -> Word
word8ToWord = fromIntegral

boxBytes :: Bytes# -> Bytes
boxBytes (# a, b, c #) = Bytes (ByteArray a) (I# b) (I# c)


bytesIndex :: Bytes# -> Int -> Word8
bytesIndex (# arr, off, _ #) ix = indexByteArray (ByteArray arr) (I# off + ix)

-- This assumes that the Bytes is longer than the index. It also does
-- not eliminate zero-length references to byte arrays.
unsafeDrop# :: Int# -> Bytes# -> Bytes#
unsafeDrop# i (# arr, off, len #) = (# arr, off +# i, len -# i #)

boxLeftovers :: Maybe# (Leftovers# s) -> Maybe (Leftovers s)
boxLeftovers (# (# #) | #) = Nothing
boxLeftovers (# | (# theBytes, stream #) #) = Just (Leftovers (boxBytes theBytes) stream)

c2w :: Char -> Word8
c2w = fromIntegral . Data.Char.ord

s2b :: String -> Bytes
s2b = packBytes . map c2w

packBytes :: [Word8] -> Bytes
packBytes ws0 = runST $ do
  marr <- newByteArray (L.length ws0)
  let go [] !_ = return ()
      go (w : ws) !ix = writeByteArray marr ix w >> go ws (ix + 1)
  go ws0 0
  arr <- unsafeFreezeByteArray marr
  return (Bytes arr 0 (sizeofByteArray arr))

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


instance Functor Parser where
  fmap = Control.Monad.liftM

instance Applicative Parser where
  pure = pureParser
  (<*>) = Control.Monad.ap

instance Monad Parser where
  return = pure
  (>>=) = bindLifted


----------------------------------
-- Trie data type and functions --
----------------------------------
-- The code dealing with tries is known to be safe. It does not
-- do anything tricky and is a straigtforward implementation
-- of a trie whose key is a list of Word8.
data Trie a = Trie (Maybe a) (Map Word8 (Trie a))
  deriving (Functor)

instance Semigroup a => Semigroup (Trie a) where
  (<>) = appendTrie

instance Semigroup a => Monoid (Trie a) where
  mempty = Trie Nothing M.empty
  mappend = (SG.<>)

appendTrie :: Semigroup a => Trie a -> Trie a -> Trie a
appendTrie (Trie v1 m1) (Trie v2 m2) = Trie
  (SG.getOption (SG.Option v1 SG.<> SG.Option v2))
  (M.unionWith appendTrie m1 m2)

trieFromList :: [(String,a)] -> Trie a
trieFromList = fmap SG.getFirst . trieFromListAppend . map (second SG.First) 

trieFromListAppend :: Semigroup a => [(String,a)] -> Trie a
trieFromListAppend = foldMap (uncurry singletonTrie)

singletonTrie :: String -> a -> Trie a
singletonTrie k v = L.foldr (\c r -> Trie Nothing (M.singleton (c2w c) r)) (Trie (Just v) M.empty) k

-- Turn a trie of parsers into a parser that consumes a key
-- in the trie and then runs the parser corresponding to the
-- matched key.
trieToParser :: Trie (Parser a) -> Parser a
trieToParser (Trie mp m) = case mp of
  Just p -> p
  Nothing -> do
    w <- anyParser
    case M.lookup w m of
      Nothing -> failureParser
      Just t -> trieToParser t

