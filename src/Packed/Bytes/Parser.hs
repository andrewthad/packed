{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}

module Packed.Bytes.Parser
  ( Parser(..)
  , StatefulParser(..)
  , Result(..)
  , run
  , bigEndianWord16
  , bigEndianWord32
  , decimalWord32
  , replicateStashIndex
  , replicate
  , take
  , mutate
  , consume
  , stash
  ) where

import Prelude hiding (replicate,take)

import Packed.Bytes (Bytes(..))
import Control.Monad.ST (runST)
import Data.Word (Word32)
import Data.Primitive (ByteArray(..),Array,MutableArray)
import GHC.ST (ST(..))
import GHC.Word (Word32(W32#),Word16(W16#))
import GHC.Int (Int(I#))
import GHC.Types (TYPE,RuntimeRep(..),IO(..),Type)
import GHC.Exts (State#,Int#,ByteArray#,Word#,(+#),(-#),(>#),
  (<#),(==#),(>=#),(*#),(<=#),
  MutableArray#,MutableByteArray#,writeArray#,unsafeFreezeArray#,newArray#,
  unsafeFreezeByteArray#,newByteArray#,and#,
  plusWord#,timesWord#,indexWord8Array#,eqWord#,andI#,
  clz8#, or#, neWord#, uncheckedShiftL#,int2Word#,word2Int#,quotInt#,
  shrinkMutableByteArray#,copyMutableByteArray#,chr#,gtWord#,
  writeWord32Array#,readFloatArray#,runRW#,ltWord#,minusWord#,quotRemInt#,
  RealWorld,coerce,remInt#)
import qualified Data.Primitive as PM

type Maybe# (a :: TYPE r) = (# (# #) | a #)
type Either# a (b :: TYPE r) = (# a | b #)

type Result# e (r :: RuntimeRep) (a :: TYPE r) =
  (# Int# , Either# e a #)

data Result e a = Result
  { resultIndex :: !Int
  , resultValue :: !(Either e a)
  } deriving (Eq,Show)

run :: Bytes -> Parser e a -> Result e a
run (Bytes (ByteArray arr) (I# off) (I# len)) (Parser (ParserLevity f)) = case f arr off (off +# len) of
  (# ix, r #) -> case r of
    (# e | #) -> Result (I# (ix -# off)) (Left e)
    (# | a #) -> Result (I# (ix -# off)) (Right a)

newtype Parser e a = Parser { getParser :: ParserLevity e 'LiftedRep a }

-- A parser that always consumes the same amount of input. Additionally,
-- it will always succeed if that amount of input is present.
data FixedParser e a = FixedParser
  Int# -- how many bytes do we need
  ( Int# -> e ) -- convert the actual number of bytes into an error
  ( Int# -> Int# )
  -- convert the actual number of bytes into the number of
  -- bytes actually consumed (should be less than the argument given it)
  ( ByteArray# -> Int# -> a )

data FixedStatefulParser e s a = FixedStatefulParser
  Int# -- how many bytes do we need
  ( Int# -> ByteArray# -> Int# -> State# s -> (# State# s, (# e, Int# #) #) )
  -- convert the actual number of bytes into an error and a
  -- number of bytes consumed. The first argument to this function is
  -- the actual number of bytes. The second is the bytearray, and the
  -- third is the offset.
  ( ByteArray# -> Int# -> State# s -> (# State# s, a #) )

data FixedStashParser e s = 
  forall a. FixedStashParser (FixedParser e a) (a -> ST s ())

newtype ParserLevity e (r :: RuntimeRep) (a :: TYPE r) = ParserLevity
  { getParserLevity ::
       ByteArray# -- input
    -> Int# -- offset
    -> Int# -- end (not length)
    -> Result# e r a
  }

-- | A parser that can interleave arbitrary 'ST' effects with parsing.
newtype StatefulParser e s a = StatefulParser
  { getStatefulParser ::
       ByteArray# -- input
    -> Int# -- offset
    -> Int# -- end (not length)
    -> State# s
    -> (# State# s, Result# e 'LiftedRep a #)
  }

fmapFixedParser :: (a -> b) -> FixedParser e a -> FixedParser e b
fmapFixedParser f (FixedParser n toError toConsumed g) = FixedParser n toError toConsumed (\arr off -> f (g arr off))

{-# INLINE fmapParser #-}
fmapParser :: (a -> b) -> Parser e a -> Parser e b
fmapParser f (Parser (ParserLevity g)) = Parser $ ParserLevity $ \arr off0 end -> case g arr off0 end of
  (# off1, r #) -> case r of
    (# e | #) -> (# off1, (# e | #) #)
    (# | a #) -> (# off1, (# | f a #) #)

mapParserError :: (c -> e) -> Parser c a -> Parser e a
mapParserError f (Parser (ParserLevity g)) = Parser $ ParserLevity $ \arr off0 end -> case g arr off0 end of
  (# off1, r #) -> case r of
    (# e | #) -> (# off1, (# f e | #) #)
    (# | a #) -> (# off1, (# | a #) #)

instance Functor (Parser e) where
  {-# INLINE fmap #-}
  -- This is written this way to improve the likelihood that the applicative
  -- rewrite rules fire.
  fmap f p = applyParser (pureParser f) p

instance Applicative (Parser e) where
  pure = pureParser
  {-# INLINE pure #-}
  (<*>) = applyParser
  {-# INLINE (<*>) #-}
  (*>) = sequenceRightParser
  {-# INLINE (*>) #-}

instance Monad (Parser e) where
  (>>=) = bindParser
  {-# INLINE (>>=) #-}

instance Functor (StatefulParser s e) where
  fmap f p = applyStatefulParser (pureStatefulParser f) p

instance Applicative (StatefulParser s e) where
  pure = pureStatefulParser
  {-# INLINE pure #-}
  (<*>) = applyStatefulParser
  {-# INLINE (<*>) #-}
  (<*) = sequenceLeftStatefulParser
  {-# INLINE (<*) #-}
  (*>) = sequenceRightStatefulParser
  {-# INLINE (*>) #-}

instance Monad (StatefulParser s e) where
  (>>=) = bindStatefulParser
  {-# INLINE (>>=) #-}


{-# NOINLINE[1] fixedParserToParser #-}
fixedParserToParser :: FixedParser e a -> Parser e a
fixedParserToParser (FixedParser n toError toConsumed f) = Parser $ ParserLevity $ \arr off end ->
  let len = end -# off 
   in case len >=# n of
        1# -> (# off +# n, (# | f arr off #) #)
        _ -> (# off +# toConsumed len, (# toError len | #) #)

{-# NOINLINE[1] fixedStatefulParserToStatefulParser #-}
fixedStatefulParserToStatefulParser :: FixedStatefulParser e s a -> StatefulParser e s a
fixedStatefulParserToStatefulParser = error "Uhoenuthantoehunt"

{-# NOINLINE[1] fixedStashParserToStatefulParser #-}
fixedStashParserToStatefulParser :: FixedStashParser e s -> StatefulParser e s ()
fixedStashParserToStatefulParser (FixedStashParser p f) =
  consume (fixedParserToParser p) >>= mutate . f

take :: e -> Int -> Parser e Bytes
take e (I# n# ) = Parser $ ParserLevity $ \arr off end -> case (end -# off) >=# n# of
  1# -> (# off +# n#, (# | Bytes (PM.ByteArray arr) (I# off) (I# n#) #) #)
  _ -> (# end, (# e | #) #)

{-# INLINE bigEndianWord32 #-}
bigEndianWord32 :: e -> Parser e Word32
bigEndianWord32 = fixedParserToParser . fixedBigEndianWord32

{-# INLINE fixedBigEndianWord32 #-}
fixedBigEndianWord32 :: e -> FixedParser e Word32
fixedBigEndianWord32 e = FixedParser 4# (\_ -> e) (\_ -> 0#) (\arr off -> W32# (unsafeBigEndianWord32Unboxed arr off))

unsafeBigEndianWord32Unboxed :: ByteArray# -> Int# -> Word#
unsafeBigEndianWord32Unboxed arr off =
  let !byteA = indexWord8Array# arr off
      !byteB = indexWord8Array# arr (off +# 1#)
      !byteC = indexWord8Array# arr (off +# 2#)
      !byteD = indexWord8Array# arr (off +# 3#)
      !theWord = uncheckedShiftL# byteA 24#
           `or#` uncheckedShiftL# byteB 16#
           `or#` uncheckedShiftL# byteC 8#
           `or#` byteD
   in theWord

{-# INLINE bigEndianWord16 #-}
bigEndianWord16 :: e -> Parser e Word16
bigEndianWord16 = fixedParserToParser . fixedBigEndianWord16

{-# INLINE fixedBigEndianWord16 #-}
fixedBigEndianWord16 :: e -> FixedParser e Word16
fixedBigEndianWord16 e = FixedParser 2# (\_ -> e) (\_ -> 0#) (\arr off -> W16# (unsafeBigEndianWord16Unboxed arr off))

unsafeBigEndianWord16Unboxed :: ByteArray# -> Int# -> Word#
unsafeBigEndianWord16Unboxed arr off =
  let !byteA = indexWord8Array# arr off
      !byteB = indexWord8Array# arr (off +# 1#)
      !theWord = uncheckedShiftL# byteA 8#
           `or#` byteB
   in theWord

-- | This parser does not allow leading zeroes. Consequently,
-- we can establish an upper bound on the number of bytes this
-- parser will consume. This means that it can typically omit
-- most bounds-checking as it runs.
decimalWord32 :: e -> Parser e Word32
decimalWord32 e = Parser (boxWord32Parser (decimalWord32Unboxed e))
  -- atMost 10#
  -- unsafeDecimalWord32Unboxed
  -- (\x -> case decimalWord32Unboxed)

decimalWord32Unboxed :: forall e. e -> ParserLevity e 'WordRep Word#
decimalWord32Unboxed e = ParserLevity $ \arr off end -> let len = end -# off in case len ># 0# of
  1# -> case unsafeDecimalDigitUnboxedMaybe arr off of
    (# (# #) | #) -> (# off, (# e | #) #)
    (# | initialDigit #) -> case initialDigit of
      0## -> -- zero is special because we do not allow leading zeroes
        case len ># 1# of
          1# -> case unsafeDecimalDigitUnboxedMaybe arr (off +# 1#) of
            (# (# #) | #) -> (# off +# 1#, (# | 0## #) #)
            (# | _ #) -> (# (off +# 2#) , (# e | #) #)
          _ -> (# off +# 1#, (# | 0## #) #)
      _ ->
        let maximumDigits = case gtWord# initialDigit 4## of
              1# -> 8#
              _ -> 9#
            go :: Int# -> Int# -> Word# -> Result# e 'WordRep Word#
            go !ix !counter !acc = case counter ># 0# of
              1# -> case ix <# end of
                1# -> case unsafeDecimalDigitUnboxedMaybe arr ix of
                  (# (# #) | #) -> (# ix, (# | acc #) #)
                  (# | w #) -> go (ix +# 1#) (counter -# 1#) (plusWord# w (timesWord# acc 10##))
                _ -> (# ix, (# | acc #) #)
              _ -> let accTrimmed = acc `and#` 0xFFFFFFFF## in case ix <# end of
                1# -> case unsafeDecimalDigitUnboxedMaybe arr ix of
                  (# (# #) | #) -> case (ltWord# accTrimmed 1000000000##) `andI#` (eqWord# initialDigit 4##) of
                    1# -> (# ix, (# e | #) #)
                    _ -> (# ix, (# | accTrimmed #) #)
                  (# | _ #) -> (# ix, (# e | #) #)
                _ -> case (ltWord# accTrimmed 1000000000##) `andI#` (eqWord# initialDigit 4##) of
                  1# -> (# ix, (# e | #) #)
                  _ -> (# ix, (# | accTrimmed #) #)
         in go ( off +# 1# ) maximumDigits initialDigit
  _ -> (# off, (# e | #) #)

unsafeDecimalDigitUnboxedMaybe :: ByteArray# -> Int# -> Maybe# Word#
unsafeDecimalDigitUnboxedMaybe arr off =
  let !w = minusWord# (indexWord8Array# arr (off +# 0#)) 48##
   in case ltWord# w 10## of
        1# -> (# | w #)
        _ -> (# (# #) | #)

{-# INLINE applyFixedParser #-}
applyFixedParser :: FixedParser e (a -> b) -> FixedParser e a -> FixedParser e b
applyFixedParser (FixedParser n1 toError1 toConsumed1 p1) (FixedParser n2 toError2 toConsumed2 p2) =
  FixedParser (n1 +# n2)
    (\i -> case i <# n1 of
      1# -> toError1 i
      _ -> toError2 (n1 -# i)
    )
    (\i -> case i <# n1 of
      1# -> toConsumed1 i
      _ -> n1 +# toConsumed2 (i -# n1)
    )
    (\arr off0 -> p1 arr off0 (p2 arr (off0 +# n1)))

{-# INLINE tupleFixedParsers #-}
tupleFixedParsers :: FixedParser e a -> FixedParser e b -> FixedParser e (a,b)
tupleFixedParsers (FixedParser n1 toError1 toConsumed1 p1) (FixedParser n2 toError2 toConsumed2 p2) =
  FixedParser (n1 +# n2)
    (\i -> case i <# n1 of
      1# -> toError1 i
      _ -> toError2 (n1 -# i)
    )
    (\i -> case i <# n1 of
      1# -> toConsumed1 i
      _ -> n1 +# toConsumed2 (i -# n1)
    )
    (\arr off0 -> (p1 arr off0, p2 arr (off0 +# n1)))
  
{-# INLINE appendFixedStashParsers #-}
appendFixedStashParsers :: FixedStashParser e s -> FixedStashParser e s -> FixedStashParser e s
appendFixedStashParsers (FixedStashParser consumeA mutateA) (FixedStashParser consumeB mutateB) =
  FixedStashParser
    (tupleFixedParsers consumeA consumeB) 
    (\(a,b) -> mutateA a *> mutateB b)

{-# RULES "parserApplyPure{Fixed}" [~2] forall f a. applyParser (pureParser f) (fixedParserToParser a) =
      fixedParserToParser (fmapFixedParser f a)
#-}
{-# RULES "parserApply{Fixed}" [~2] forall f a. applyParser (fixedParserToParser f) (fixedParserToParser a) =
      fixedParserToParser (applyFixedParser f a)
#-}
{-# RULES "parserApplyReassociate" [~2] forall f a b. applyParser (applyParser f (fixedParserToParser a)) (fixedParserToParser b) =
      applyParser
        (fmapParser uncurry f)
        (fixedParserToParser (tupleFixedParsers a b))
#-}
{-# RULES "parserApplyBindReassociate" [2] forall f a b. applyParser (fixedParserToParser a) (bindParser (fixedParserToParser b) f) =
      bindParser
        (fixedParserToParser (tupleFixedParsers a b))
        (\(g,y) -> fmapParser g (f y))
#-}
{-# RULES "stashSequenceRight" [~2] forall a b. sequenceRightStatefulParser (fixedStashParserToStatefulParser a) (fixedStashParserToStatefulParser b) =
      fixedStashParserToStatefulParser (appendFixedStashParsers a b)
#-}
{-# RULES "stashSequenceLeft" [~2] forall a b. sequenceLeftStatefulParser (fixedStashParserToStatefulParser a) (fixedStashParserToStatefulParser b) =
      fixedStashParserToStatefulParser (appendFixedStashParsers a b)
#-}

{-# NOINLINE[1] pureParser #-}
pureParser :: a -> Parser e a
pureParser a = Parser (ParserLevity (\_ off _ -> (# off, (# | a #) #)))

{-# NOINLINE[1] pureStatefulParser #-}
pureStatefulParser :: a -> StatefulParser e s a
pureStatefulParser a = StatefulParser (\_ off _ s0 -> (# s0, (# off, (# | a #) #) #))

{-# NOINLINE[1] applyParser #-}
applyParser :: Parser e (a -> b) -> Parser e a -> Parser e b
applyParser (Parser f) (Parser g) = Parser (applyLifted f g)

{-# NOINLINE[1] sequenceRightParser #-}
sequenceRightParser :: Parser e a -> Parser e b -> Parser e b
sequenceRightParser (Parser a) (Parser b) = Parser (liftedSequenceRight a b)

{-# NOINLINE[1] bindParser #-}
bindParser :: Parser e a -> (a -> Parser e b) -> Parser e b
bindParser (Parser a) f = Parser (bindLifted a (\x -> getParser (f x)))

{-# NOINLINE[1] boxWord32Parser #-}
boxWord32Parser ::
     ParserLevity e 'WordRep Word#
  -> ParserLevity e 'LiftedRep Word32
boxWord32Parser (ParserLevity f) = ParserLevity $ \arr off0 end -> case f arr off0 end of
  (# off1, r #) -> case r of
    (# e | #) -> (# off1, (# e | #) #)
    (# | w #) -> (# off1, (# | W32# w #) #)

{-# NOINLINE[1] applyStatefulParser #-}
applyStatefulParser :: 
     StatefulParser e s (a -> b)
  -> StatefulParser e s a
  -> StatefulParser e s b
applyStatefulParser (StatefulParser f) (StatefulParser g) = StatefulParser $ \arr off0 end s0 -> case f arr off0 end s0 of
  (# s1, (# off1, r #) #) -> case r of
    (# e | #) -> (# s1, (# off1, (# e | #) #) #)
    (# | a #) -> case g arr off1 end s1 of
      (# s2, (# off2, r2 #) #) -> case r2 of
        (# e | #) -> (# s2, (# off2, (# e | #) #) #)
        (# | b #) -> (# s2, (# off2, (# | a b #) #) #)

{-# NOINLINE[1] sequenceLeftStatefulParser #-}
sequenceLeftStatefulParser :: 
     StatefulParser e s a
  -> StatefulParser e s b
  -> StatefulParser e s a
sequenceLeftStatefulParser (StatefulParser f) (StatefulParser g) = StatefulParser $ \arr off0 end s0 -> case f arr off0 end s0 of
  (# s1, (# off1, r #) #) -> case r of
    (# e | #) -> (# s1, (# off1, (# e | #) #) #)
    (# | a #) -> case g arr off1 end s1 of
      (# s2, (# off2, r2 #) #) -> case r2 of
        (# e | #) -> (# s2, (# off2, (# e | #) #) #)
        (# | _ #) -> (# s2, (# off2, (# | a #) #) #)

{-# NOINLINE[1] sequenceRightStatefulParser #-}
sequenceRightStatefulParser :: 
     StatefulParser e s a
  -> StatefulParser e s b
  -> StatefulParser e s b
sequenceRightStatefulParser (StatefulParser f) (StatefulParser g) = StatefulParser $ \arr off0 end s0 -> case f arr off0 end s0 of
  (# s1, (# off1, r #) #) -> case r of
    (# e | #) -> (# s1, (# off1, (# e | #) #) #)
    (# | _ #) -> case g arr off1 end s1 of
      (# s2, (# off2, r2 #) #) -> case r2 of
        (# e | #) -> (# s2, (# off2, (# e | #) #) #)
        (# | b #) -> (# s2, (# off2, (# | b #) #) #)

applyLifted :: 
     ParserLevity e 'LiftedRep (a -> b)
  -> ParserLevity e 'LiftedRep a
  -> ParserLevity e 'LiftedRep b
applyLifted (ParserLevity f) (ParserLevity g) = ParserLevity $ \arr off0 end -> case f arr off0 end of
  (# off1, r #) -> case r of
    (# e | #) -> (# off1, (# e | #) #)
    (# | a #) -> case g arr off1 end of
      (# off2, r2 #) -> case r2 of
        (# e | #) -> (# off2, (# e | #) #)
        (# | b #) -> (# off2, (# | a b #) #)

liftedSequenceRight ::
     ParserLevity e 'LiftedRep a
  -> ParserLevity e 'LiftedRep b
  -> ParserLevity e 'LiftedRep b
liftedSequenceRight (ParserLevity f) (ParserLevity g) =
  ParserLevity $ \arr off0 end -> case f arr off0 end of
    (# off1, r #) -> case r of
      (# e | #) -> (# off1, (# e | #) #)
      (# | _ #) -> case g arr off1 end of
        (# off2, r2 #) -> case r2 of
          (# e | #) -> (# off2, (# e | #) #)
          (# | b #) -> (# off2, (# | b #) #)

bindLifted ::
     ParserLevity e 'LiftedRep a
  -> (a -> ParserLevity e 'LiftedRep b)
  -> ParserLevity e 'LiftedRep b
bindLifted (ParserLevity x) f = ParserLevity $ \arr off0 end -> case x arr off0 end of
  (# off1, r #) -> case r of
    (# e | #) -> (# off1, (# e | #) #)
    (# | a #) -> case getParserLevity (f a) arr off1 end of
      (# off2, r2 #) -> case r2 of
        (# e | #) -> (# off2, (# e | #) #)
        (# | b #) -> (# off2, (# | b #) #)

bindStatefulParser ::
     StatefulParser e s a
  -> (a -> StatefulParser e s b)
  -> StatefulParser e s b
bindStatefulParser (StatefulParser x) f =
  StatefulParser $ \arr off0 end s0 -> case x arr off0 end s0 of
    (# s1, (# off1, r #) #) -> case r of
      (# e | #) -> (# s1, (# off1, (# e | #) #) #)
      (# | a #) -> case getStatefulParser (f a) arr off1 end s1 of
        (# s2, (# off2, r2 #) #) -> case r2 of
          (# e | #) -> (# s2, (# off2, (# e | #) #) #)
          (# | b #) -> (# s2, (# off2, (# | b #) #) #)

-- | Lift a 'ST' action into a stateful parser.
mutate :: ST s a -> StatefulParser e s a
mutate (ST f) = StatefulParser $ \_ off _ s0 ->
  case f s0 of
    (# s1, a #) -> (# s1, (# off, (# | a #) #) #)

-- | Lift a pure parser into a stateful parser.
consume :: Parser e a -> StatefulParser e s a
consume (Parser (ParserLevity f)) = StatefulParser $ \arr off end s0 ->
  (# s0, f arr off end #)

-- | Run a parser and then feed its result into the @ST@ action. Note
-- that:
--
-- > stash p f = consume p >>= mutate . f
--
-- However, @stash@ is eligible for a few additional rewrite rules
-- and should be preferred when possible.
stash :: Parser e a -> (a -> ST s ()) -> StatefulParser e s ()
-- This might actually not be needed. Rethink this.
stash p f = consume p >>= mutate . f
{-# NOINLINE[1] stash #-}
{-# RULES "stash{Fixed}" [~2] forall p f. stash (fixedParserToParser p) f = fixedStashParserToStatefulParser (FixedStashParser p f) #-}

{-# NOINLINE[1] replicateStashIndex #-}
{-# RULES "replicateStashIndex{Fixed}" [~2] forall toErr n p save. replicateStashIndex toErr n (fixedParserToParser p) save =
   fixedStatefulParserToStatefulParser (replicateFixedStashIndex toErr n p save)
#-}
replicateStashIndex :: forall e c s a.
     (Int -> c -> e) -- ^ Turn the index into an error message
  -> Int -- ^ Number of times to run the parser
  -> Parser c a -- ^ Parser
  -> (Int -> a -> ST s ()) -- ^ Save the result of a successful parse
  -> StatefulParser e s ()
replicateStashIndex toErr n p save = go 0 where
  go !ix = if ix < n
    then (consume (mapParserError (toErr ix) p) >>= (mutate . save ix)) *> go (ix + 1)
    else pure ()
  
replicateFixedStashIndex :: forall e c s a.
     (Int -> c -> e) -- ^ Turn the index into an error message
  -> Int -- ^ Number of times to run the parser
  -> FixedParser c a -- ^ Parser
  -> (Int -> a -> ST s ()) -- ^ Save the result of a successful parse
  -> FixedStatefulParser e s ()
replicateFixedStashIndex castErr (I# n) (FixedParser sz toErr toConsumed f) save =
  FixedStatefulParser (n *# sz)
    ( \len arr off s ->
      let !(# m, remaining #) = quotRemInt# len sz
          go !ix !off0 s0 = case ix <# m of
            1# -> case unST (save (I# ix) (f arr off0)) s0 of
              (# s1, _ #) -> go (ix +# 1#) (off0 +# sz) s1
            _ -> (# s0, (# castErr (I# ix) (toErr remaining), (len +# toConsumed remaining) -# sz #) #)
       in go 0# off s
    )
    ( \arr off s ->
      let go !ix !off0 s0 = case ix <# n of
            1# -> case unST (save (I# ix) (f arr off0)) s0 of
              (# s1, _ #) -> go (ix +# 1#) (off0 +# sz) s1
            _ -> (# s0, () #)
       in go 0# off s
    )

{-# NOINLINE[1] replicate #-}
{-# RULES "replicate{Fixed}" [~2] forall n p. replicate n (fixedParserToParser p) =
   fixedParserToParser (replicateFixed n p)
#-}
replicate :: Int -> Parser e a -> Parser e (Array a)
replicate n p = go 0 [] where
  go ix !xs = if ix < n
    then do
      x <- p
      go (ix + 1) (x : xs)
    else return (reverseArrayFromListN n xs)

replicateFixed :: Int -> FixedParser e a -> FixedParser e (Array a)
replicateFixed (I# n) (FixedParser sz toErr toConsumed f) =
  FixedParser (n *# sz)
    (\len -> toErr (remInt# len sz))
    (\len -> (len +# toConsumed (remInt# len sz)) -# sz)
    (\arr off ->
      let go !ix !off0 !xs = case ix <# n of
            1# -> go (ix +# 1#) (off0 +# sz) (f arr off0 : xs)
            _ -> xs
       in reverseArrayFromListN (I# n) (go 0# off [])
    )

unST :: ST s a -> State# s -> (# State# s, a #)
unST (ST f) = f

-- Precondition: the first argument must be the length of the list.
reverseArrayFromListN :: Int -> [a] -> Array a
reverseArrayFromListN n l =
  createArray n errorThunk $ \mi ->
    let go !i (x:xs) = do
          PM.writeArray mi i x
          go (i - 1) xs
        go !_ [] = return ()
     in go (n - 1) l

{-# NOINLINE errorThunk #-}
errorThunk :: a
errorThunk = error "Packed.Bytes.Parser: error thunk forced"

createArray
  :: Int
  -> a
  -> (forall s. MutableArray s a -> ST s ())
  -> Array a
createArray n x f = runST $ do
  ma <- PM.newArray n x
  f ma
  PM.unsafeFreezeArray ma
