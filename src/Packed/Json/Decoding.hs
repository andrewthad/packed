{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnboxedTuples #-}

module Packed.Json.Decoding
  ( FromJson(..)
  , JsonDecoding(..)
  , JsonError(..)
  , Value(..)
  , Context(..) -- stop exporting this
  , valueParser
  , decode
  , decodeStreamST
  , key
  , object
  ) where

import Control.Monad (forM_)
import Control.Monad.ST (runST,ST)
import Data.Char (ord)
import Data.Coerce (coerce)
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.Primitive (Array,PrimArray,Prim,SmallArray,UnliftedArray)
import Data.Primitive.SmallArray.Maybe (SmallMaybeArray)
import Data.Ratio ((%))
import Data.Type.Coercion (Coercion(..))
import Data.Word (Word8)
import GHC.Exts (Any,Int#,Int(I#))
import Packed.Bytes (Bytes(..))
import Packed.Bytes.Parser (Parser,PureResult(..))
import Packed.Bytes.Small (ByteArray)
import Packed.Bytes.Stream.ST (ByteStream)
import Packed.Bytes.Trie (Trie)
import Packed.Text (Text(..))
import Packed.Text.Small (SmallText)
import Unsafe.Coerce (unsafeCoerce)

import qualified Data.List as L
import qualified Data.Primitive as PM
import qualified Data.Primitive.SmallArray.Maybe as PSAM
import qualified Data.Type.Coercion as C
import qualified Packed.Bytes as B
import qualified Packed.Bytes.Parser as P
import qualified Packed.Bytes.Trie as T
import qualified Packed.Text.Small as TS

data JsonError
  = JsonErrorIndex !Int !JsonError
  | JsonErrorKey !Text !JsonError
  | JsonErrorMissing !SmallText
  | JsonErrorUndocumented
  deriving (Show,Eq)

data Error
  = ErrorMissing !SmallText
  | ErrorUndocumented

data ContextUnit
  = ContextUnitKey !Text
  | ContextUnitSmallKey !SmallText
  | ContextUnitIndex !Int

data Context
  = ContextCons !ContextUnit !Context
  | ContextNil

data Value
  = ValueArray !(Array Value)
  | ValueObject !(Array (Text,Value)) -- fix this, choose a better map type
  | ValueNumber !Rational -- at some point, choose a better number type
  | ValueNull
  | ValueString !Text
  | ValueBool !Bool
  deriving (Show,Eq)

class FromJson a where
  fromJson :: JsonDecoding a

instance FromJson Value where
  fromJson = JsonDecodingRaw Just

instance FromJson Int where
  fromJson = JsonDecodingGround GroundInt

instance FromJson Word where
  fromJson = JsonDecodingGround GroundWord

instance FromJson Bool where
  fromJson = JsonDecodingBool

instance FromJson Text where
  fromJson = JsonDecodingText

data JsonDecoding :: Type -> Type where
  JsonDecodingRaw :: (Value -> Maybe a) -> JsonDecoding a
  JsonDecodingArray :: JsonDecoding a -> JsonDecoding (Array a)
  JsonDecodingPrimArray :: Prim a => JsonDecoding a -> JsonDecoding (PrimArray a)
  JsonDecodingGroundArray :: Ground a -> JsonDecoding (PrimArray a)
  JsonDecodingCoerce :: Coercion a b -> JsonDecoding b -> JsonDecoding a
  JsonDecodingGround :: Ground a -> JsonDecoding a
  JsonDecodingBool :: JsonDecoding Bool
  JsonDecodingText :: JsonDecoding Text
  JsonDecodingObject :: FastMapDecoding a -> JsonDecoding a
  -- The object constructor is an extremely efficent way to decode a json
  -- object to a record type without building a hashmap as an intermediate
  -- data structure. 
  JsonDecodingArrayLeftFold :: JsonDecoding b -> (a -> b -> b) -> a -> JsonDecoding a
  JsonDecodingFunctor :: (b -> a) -> JsonDecoding b -> JsonDecoding a
  JsonDecodingMapLeftFold :: !(Array (JsonDecoding b)) -> Trie (UnsafeIndexWith k) -> (a -> k -> b -> b) -> a -> JsonDecoding a
  -- The map-left-fold data constructor is for situations in which a user
  -- wants to fold over a json object and create something that uses a variable
  -- number of the key-value pairs (likely all of them).

  -- rethink map left fold
  -- JsonDecodingMapLeftFold :: (Text -> Maybe (k,JsonDecoding b)) -> (a -> k -> b -> b) -> a -> JsonDecoding a
  -- JsonDecodingMapFix :: (d -> c -> c) -> (Text -> Maybe (c,Maybe (JsonDecoding b))) -> d -> (a -> c -> b -> b) -> JsonDecoding a

data UnsafeIndexWith k = UnsafeIndexWith !Int k

data Ground :: Type -> Type where
  GroundInt :: Ground Int
  GroundWord :: Ground Word

optimize :: JsonDecoding a -> JsonDecoding a
optimize = go where
  go :: JsonDecoding b -> JsonDecoding b
  go (JsonDecodingRaw f) = JsonDecodingRaw f
  go (JsonDecodingFunctor f d) = JsonDecodingFunctor f (optimize d)
  go (JsonDecodingGround g) = JsonDecodingGround g
  go (JsonDecodingPrimArray d) = case optimize d of
    JsonDecodingGround g -> JsonDecodingGroundArray g
    JsonDecodingCoerce c (JsonDecodingGround g) -> error "Uhoeunhta"
    x -> JsonDecodingPrimArray x
  go (JsonDecodingCoerce c d) = case optimize d of
    JsonDecodingCoerce c' d' -> JsonDecodingCoerce (C.trans c c') d'
    x -> JsonDecodingCoerce c x

data MapDecoding a where
  MapDecodingPure ::
       !a -- function
    -> MapDecoding a
  MapDecodingApply ::
       !SmallText
    -> !ByteArray
    -> JsonDecoding a
    -> Maybe a -- a default value if the key is missing
    -> !(MapDecoding (a -> b)) -- next decoding
    -> MapDecoding b

instance Functor MapDecoding where
  fmap f (MapDecodingPure a) = MapDecodingPure (f a)
  fmap f (MapDecodingApply a b c d y) = MapDecodingApply a b c d ((f .) <$> y)

instance Applicative MapDecoding where
  pure = MapDecodingPure
  MapDecodingPure f <*> y = fmap f y
  MapDecodingApply a b c d y <*> z = MapDecodingApply a b c d (flip <$> y <*> z)

key :: SmallText -> JsonDecoding a -> Maybe a -> MapDecoding a
key theKey dec mdef = MapDecodingApply theKey (TS.encodeUtf8 theKey) dec mdef (MapDecodingPure id)

data FastMapDecoding a = FastMapDecoding
  !Int -- arity of function of type X -> Y -> ... -> a
  Any -- the function of unknown arity whose return type is a
  (Trie (Indexed (JsonDecoding Any))) -- trie
  !(SmallMaybeArray Any) -- default values
  !(UnliftedArray SmallText) -- Names of keys. Used for informative errors.

data Indexed a = Indexed !Int a

object :: MapDecoding a -> JsonDecoding a
object = JsonDecodingObject . accelerateMapDecoding

lengthMapDecoding :: MapDecoding a -> Int
lengthMapDecoding = go 0 where
  go :: Int -> MapDecoding b -> Int
  go !n (MapDecodingPure _) = n
  go !n (MapDecodingApply _ _ _ _ xs) = go (n + 1) xs

-- Internally, this function must use unsafeCoerce.
accelerateMapDecoding :: MapDecoding a -> FastMapDecoding a
accelerateMapDecoding md = unsafeFromAnyFastMapDecoding (go [] [] [] (sz - 1) md) where
  sz = lengthMapDecoding md
  go :: [(Bytes,Indexed (JsonDecoding Any))] -- decodings
     -> [Indexed Any] -- default values
     -> [SmallText] -- key names
     -> Int -- function argument count
     -> MapDecoding b
     -> FastMapDecoding Any
  go !decs !defs !keys !ix (MapDecodingApply t b dec mdef xs) = go
    ((B.fromByteArray b,Indexed ix (unsafeToAnyJsonDecoding dec)) : decs)
    (maybe defs (\y -> Indexed ix (unsafeToAny y) : defs) mdef)
    (t : keys)
    (ix - 1)
    xs
  go !decs !defs !keys !_ (MapDecodingPure f) =
    let defArray = runST $ do
          marr <- PSAM.newSmallMaybeArray sz Nothing
          forM_ defs $ \(Indexed ixV v) -> do
            PSAM.writeSmallMaybeArray marr ixV (Just v)
          PSAM.unsafeFreezeSmallMaybeArray marr
     in FastMapDecoding sz (unsafeToAny f) (T.fromList decs) defArray (PM.unliftedArrayFromList (L.reverse keys))

unsafeFromAnyFastMapDecoding :: FastMapDecoding Any -> FastMapDecoding a
unsafeFromAnyFastMapDecoding = unsafeCoerce

unsafeToAnyJsonDecoding :: JsonDecoding a -> JsonDecoding Any
unsafeToAnyJsonDecoding = unsafeCoerce

unsafeToAny :: a -> Any
unsafeToAny = unsafeCoerce

unsafeFromAny :: Any -> a
unsafeFromAny = unsafeCoerce

decode :: JsonDecoding a -> Bytes -> Either JsonError a
decode decoding bytes = do
  let PureResult _ e c = P.parseBytes bytes ContextNil (decodingToParser decoding)
   in case e of
        Right a -> Right a
        Left m -> Left (prepareContextError c (fromMaybe ErrorUndocumented m))

decodeStreamST :: JsonDecoding a -> ByteStream s -> ST s (Either (JsonError,Maybe (P.Leftovers s)) a)
decodeStreamST decoding bytes = do
  P.Result mleftovers e c <- P.parseStreamST bytes ContextNil (decodingToParser decoding <* P.endOfInput)
  return $ case e of
    Right a -> Right a
    Left m -> Left (prepareContextError c (fromMaybe ErrorUndocumented m),mleftovers)

prepareContextError :: Context -> Error -> JsonError
prepareContextError c e = go c (prepareError e) where
  go ContextNil !j = j
  go (ContextCons u cnext) !j = go cnext (layerContextUnit u j)

layerContextUnit :: ContextUnit -> JsonError -> JsonError
layerContextUnit u je = case u of
  ContextUnitKey theKey -> JsonErrorKey theKey je
  ContextUnitIndex ix -> JsonErrorIndex ix je

prepareError :: Error -> JsonError
prepareError = \case
  ErrorMissing theKey -> JsonErrorMissing theKey
  ErrorUndocumented -> JsonErrorUndocumented

-- This only behaves correctly when there are no
-- characters that require escaping in any of the expected
-- keys and when the encoded key does not use unicode
-- escaping syntax.
consumeKey ::
     Trie (Indexed (JsonDecoding Any))
  -> Parser e c (Either (Trie (Indexed (JsonDecoding Any))) (Indexed (JsonDecoding Any)))
consumeKey = T.lookupM
  P.any
  (P.bytes . B.fromByteArray)
  (fmap (== charToWord8 '"') P.peek)
  return
  

fastMapDecodingToParser :: FastMapDecoding a -> Parser Error Context a
fastMapDecodingToParser (FastMapDecoding n f decs defs keys) = do
  P.skipSpace
  P.byte (charToWord8 '{')
  P.skipSpace
  maybeVals <- P.any >>= \case
    125 -> do -- close curly brace
      P.skipSpace
      return defs
    34 -> P.statefully $ do -- double quote
      mutVals <- P.mutation (PSAM.thawSmallMaybeArray defs 0 n)
      let go = do
            -- Currently, we do not correctly skip over unneeded keys.
            -- This must be fixed
            Indexed ix valParser <- P.consumption (P.triePure decs)
            let textualKey = PM.indexUnliftedArray keys ix
            val <- P.consumption (P.byte (charToWord8 '"') *> P.skipSpace *> P.byte (charToWord8 ':') *> P.scoped (contextConsSmallKey textualKey) (decodingToParser valParser) <* P.skipSpace)
            P.mutation (PSAM.writeSmallMaybeArray mutVals ix (Just val))
            P.consumption P.any >>= \case
              125 -> do -- close curly brace
                P.consumption P.skipSpace
                P.mutation (PSAM.unsafeFreezeSmallMaybeArray mutVals)
              44 -> P.consumption (P.skipSpace *> P.byte (charToWord8 '"')) *> go -- comma
              _ -> P.consumption P.failure
      go
    _ -> P.failure
  case PSAM.sequenceSmallMaybeArray maybeVals of
    Nothing -> do -- a value was missing
      let ix = findNothing maybeVals
      let theKey = PM.indexUnliftedArray keys ix
      P.failureDocumented (ErrorMissing theKey)
    Just confirmed ->
      return (unsafeFromAny (unsafeApplyFunction f n confirmed))

unsafeApplyFunction ::
     Any -- untyped function
  -> Int -- arity of function
  -> SmallArray Any -- arguments
  -> Any 
unsafeApplyFunction f n args = case n of
  0 -> f
  1 ->
    let (# a1 #) = PM.indexSmallArray## args 0
     in unsafeFromAny f a1
  2 ->
    let (# a1 #) = PM.indexSmallArray## args 0
        (# a2 #) = PM.indexSmallArray## args 1
     in unsafeFromAny f a1 a2
  3 ->
    let (# a1 #) = PM.indexSmallArray## args 0
        (# a2 #) = PM.indexSmallArray## args 1
        (# a3 #) = PM.indexSmallArray## args 2
     in unsafeFromAny f a1 a2 a3
  _ -> error "figure out more arguments"

-- If no Nothing is found, this returns (-1)
findNothing :: SmallMaybeArray a -> Int
findNothing a = go 0 where
  go !ix = if ix < PSAM.sizeofSmallMaybeArray a
    then case PSAM.indexSmallMaybeArray a ix of
      Nothing -> ix
      Just _ -> go (ix + 1)
    else error "Packed.Json.Decoding.findNothing: failed"

decodingToParser :: JsonDecoding a -> Parser Error Context a
decodingToParser = \case
  JsonDecodingRaw f -> do
    v <- valueParser
    maybe P.failure pure (f v)
  JsonDecodingCoerce Coercion d -> coerce (decodingToParser d)
  JsonDecodingBool -> (P.skipSpace *> P.any) >>= \case
    116 -> P.byte (charToWord8 'r')
        *> P.byte (charToWord8 'u')
        *> P.byte (charToWord8 'e')
        *> pure True
        <* P.skipSpace
    102 -> P.byte (charToWord8 'a')
        *> P.byte (charToWord8 'l')
        *> P.byte (charToWord8 's')
        *> P.byte (charToWord8 'e')
        *> pure False
        <* P.skipSpace
    _ -> P.failure
  JsonDecodingText ->
    P.skipSpace *> P.byte (charToWord8 '"') *> stringParserAfterQuote <* P.skipSpace
  JsonDecodingArray d -> do
    P.skipSpace
    P.byte (charToWord8 '[')
    vals <- P.replicateIntersperseByteIndex# contextConsIndex (charToWord8 ',') (decodingToParser d)
    P.byte (charToWord8 ']')
    P.skipSpace
    pure vals 
  JsonDecodingObject fmd -> fastMapDecodingToParser fmd
  JsonDecodingGround g -> case g of
    GroundWord ->
      -- Fix this. It needs to handle things with exponents. 
      P.skipSpace *> P.decimalWord <* P.skipSpace
  JsonDecodingGroundArray g -> case g of
    GroundWord ->
         P.skipSpace
      *> P.byte (charToWord8 '[')
      *> P.replicateIntersperseBytePrim
           (charToWord8 ',')
           (P.skipSpace *> P.decimalWord <* P.skipSpace)
      <* P.byte (charToWord8 ']')
      <* P.skipSpace
    GroundInt ->
         P.skipSpace
      *> P.byte (charToWord8 '[')
      *> P.replicateIntersperseBytePrim
           (charToWord8 ',')
           (P.skipSpace *> P.decimalInt <* P.skipSpace)
      <* P.byte (charToWord8 ']')
      <* P.skipSpace

valueParser :: Parser e Context Value
valueParser = do
  P.skipSpace
  x <- P.any
  v <- case x of
    116 -> P.byte (charToWord8 'r')
        *> P.byte (charToWord8 'u')
        *> P.byte (charToWord8 'e')
        *> pure (ValueBool True)
    102 -> P.byte (charToWord8 'a')
        *> P.byte (charToWord8 'l')
        *> P.byte (charToWord8 's')
        *> P.byte (charToWord8 'e')
        *> pure (ValueBool False)
    110 -> P.byte (charToWord8 'u')
        *> P.byte (charToWord8 'l')
        *> P.byte (charToWord8 'l')
        *> pure ValueNull
    34 -> do
      s <- stringParserAfterQuote
      pure (ValueString s)
    91 -> do
      vals <- P.replicateIntersperseByteIndex# contextConsIndex (charToWord8 ',') valueParser
      P.byte (charToWord8 ']')
      pure (ValueArray vals) 
    123 -> do
      vals <- P.replicateIntersperseByte (charToWord8 ',') $ do
        P.skipSpace
        P.byte (charToWord8 '"')
        theKey <- stringParserAfterQuote
        P.byte (charToWord8 ':')
        val <- P.scoped (contextConsKey theKey) valueParser
        pure (theKey,val) 
      P.skipSpace
      P.byte (charToWord8 '}')
      pure (ValueObject vals)
    45 -> do
      w <- P.decimalDigitWord
      fmap ValueNumber (parserDecimalRational False (fromIntegral w))
    _ -> do
      let w = word8ToWord x - 48
      if w < 10
        then if w > 0
          then fmap ValueNumber (parserDecimalRational True (fromIntegral w))
          else do
            byt <- P.peek -- should use some kind of peekMaybe
            case byt of
              46 -> do
                _ <- P.any
                i <- P.decimalDigitWord
                fractionalPart <- parserFractionalPart (fromIntegral i)
                c <- P.peek -- should use a peekMaybe
                if c == 69 || c == 109
                  then do
                    _ <- P.any
                    e <- exponentAfterE
                    pure (ValueNumber (fractionalPart * (tenExp e)))
                  else pure (ValueNumber fractionalPart)
              69 -> do
                _ <- P.any
                _ <- P.optionalPlusMinus
                P.skipDigits
                pure (ValueNumber 0)
              101 -> do
                _ <- P.any
                _ <- P.optionalPlusMinus
                P.skipDigits
                pure (ValueNumber 0)
              _ -> pure (ValueNumber 0)
        else P.failure
  P.skipSpace
  pure v
    
-- Fix this. It needs to validate that the bytes are UTF-8 encoded
-- text.
stringParserAfterQuote :: Parser e c Text
stringParserAfterQuote = do
  Bytes arr off len <- P.takeBytesUntilByteConsume (charToWord8 '"')
  pure (Text arr (fromIntegral off) len)


charToWord8 :: Char -> Word8
charToWord8 = fromIntegral . ord

word8ToWord :: Word8 -> Word
word8ToWord = fromIntegral

parserDecimalRational :: Bool -> Integer -> Parser e c Rational
parserDecimalRational isPositive initialDigit = do
  wholePart <- parserPositiveInteger initialDigit
  k <- P.peek >>= \case
    46 -> do
      _ <- P.any
      i <- P.decimalDigitWord
      fractionalPart <- parserFractionalPart (fromIntegral i)
      c <- P.peek
      if c == 69 || c == 101
        then do
          _ <- P.any
          e <- exponentAfterE
          pure (((wholePart % 1) + fractionalPart) * (tenExp e))
        else pure ((wholePart % 1) + fractionalPart)
    101 -> do
      _ <- P.any
      e <- exponentAfterE
      pure ((wholePart % 1) * (tenExp e))
    69 -> do
      _ <- P.any
      e <- exponentAfterE
      pure ((wholePart % 1) * (tenExp e))
    _ -> pure (wholePart % 1)
  pure (if isPositive then k else negate k)

tenExp :: Integer -> Rational
tenExp x = if x > 0 then 10 ^ x else 0.1 ^ negate x

-- argument should be between 0 and 9.
parserFractionalPart :: Integer -> Parser e c Rational
parserFractionalPart = go 10 where
  go !denominator !numerator = do
    P.optionalDecimalDigitWord >>= \case
      Nothing -> pure (numerator % denominator)
      Just d -> go (denominator * 10) (numerator * 10 + fromIntegral d)
  
parserPositiveInteger :: Integer -> Parser e c Integer
parserPositiveInteger = go where
  go !i = P.optionalDecimalDigitWord >>= \case
    Nothing -> pure i
    Just j -> go (i * 10 + fromIntegral j)

exponentAfterE :: Parser e c Integer
exponentAfterE = do
  isPositive <- P.optionalPlusMinus
  i <- P.decimalDigitWord
  k <- parserPositiveInteger (fromIntegral i)
  pure (if isPositive then k else negate k)
  
contextConsIndex :: Int# -> Context -> Context
contextConsIndex i = ContextCons (ContextUnitIndex (I# i))

contextConsKey :: Text -> Context -> Context
contextConsKey theKey = ContextCons (ContextUnitKey theKey)

contextConsSmallKey :: SmallText -> Context -> Context
contextConsSmallKey theKey = ContextCons (ContextUnitSmallKey theKey)


