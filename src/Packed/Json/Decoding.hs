{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Packed.Json.Decoding
  ( JsonDecoding(..)
  , Value(..)
  , valueParser
  ) where

import Data.Char (ord)
import Data.Coerce (coerce)
import Data.Kind (Type)
import Data.Primitive (Array,PrimArray,Prim)
import Data.Ratio ((%))
import Data.Type.Coercion (Coercion(..))
import Data.Word (Word8)
import GHC.Exts (Any)
import Packed.Bytes (Bytes(..))
import Packed.Bytes.Parser
import Packed.Bytes.Trie (Trie)
import Packed.Text (Text(..))
import Packed.Text.Small (SmallText)
import Data.Maybe (fromMaybe)

import qualified Data.Type.Coercion as C
import qualified Packed.Bytes.Parser as P

data JsonError
  = JsonErrorIndex !Int !JsonError
  | JsonErrorKey !SmallText !JsonError
  | JsonErrorMissing !SmallText
  | JsonErrorUndocumented

data Error
  = ErrorMissing !SmallText
  | ErrorUndocumented

data ContextUnit
  = ContextUnitKey !SmallText
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

data JsonDecoding :: Type -> Type where
  JsonDecodingRaw :: (Value -> Maybe a) -> JsonDecoding a
  JsonDecodingArray :: JsonDecoding a -> JsonDecoding (Array a)
  JsonDecodingPrimArray :: Prim a => JsonDecoding a -> JsonDecoding (PrimArray a)
  JsonDecodingGroundArray :: Ground a -> JsonDecoding (PrimArray a)
  JsonDecodingCoerce :: Coercion a b -> JsonDecoding b -> JsonDecoding a
  JsonDecodingGround :: Ground a -> JsonDecoding a
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
       !Bytes
    -> JsonDecoding a
    -> Maybe a -- a default value if the key is missing
    -> !(MapDecoding (a -> b)) -- next decoding
    -> MapDecoding b

data FastMapDecoding a = FastMapDecoding
  !Int -- arity of function of type X -> Y -> ... -> a
  Any -- the function of unknown arity whose return type is a
  (Trie (JsonDecoding Any)) -- trie

decode :: JsonDecoding a -> Bytes -> Either JsonError a
decode decoding bytes = do
  let PureResult _ e c = parseBytes bytes ContextNil (decodingToParser decoding)
   in case e of
        Right a -> Right a
        Left m -> Left (prepareContextError c (fromMaybe ErrorUndocumented m))

prepareContextError :: Context -> Error -> JsonError
prepareContextError c e = go c (prepareError e) where
  go ContextNil !j = j
  go (ContextCons u cnext) !j = go cnext (layerContextUnit u j)

layerContextUnit :: ContextUnit -> JsonError -> JsonError
layerContextUnit u je = case u of
  ContextUnitKey key -> JsonErrorKey key je
  ContextUnitIndex ix -> JsonErrorIndex ix je

prepareError :: Error -> JsonError
prepareError = \case
  ErrorMissing key -> JsonErrorMissing key
  ErrorUndocumented -> JsonErrorUndocumented

decodingToParser :: JsonDecoding a -> Parser Error Context a
decodingToParser = \case
  JsonDecodingRaw f -> do
    v <- valueParser
    maybe P.failure pure (f v)
  JsonDecodingCoerce Coercion d -> coerce (decodingToParser d)
  JsonDecodingArray d -> do
    P.skipSpace
    P.byte (charToWord8 '[')
    vals <- P.replicateIntersperseByte (charToWord8 ',') (decodingToParser d)
    P.byte (charToWord8 ']')
    P.skipSpace
    pure vals 
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

valueParser :: Parser e c Value
valueParser = do
  skipSpace
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
      vals <- P.replicateIntersperseByte (charToWord8 ',') valueParser
      P.byte (charToWord8 ']')
      pure (ValueArray vals) 
    123 -> do
      vals <- P.replicateIntersperseByte (charToWord8 ',') $ do
        P.skipSpace
        P.byte (charToWord8 '"')
        key <- stringParserAfterQuote
        P.byte (charToWord8 ':')
        val <- valueParser
        pure (key,val) 
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
  Bytes arr off len <- takeBytesUntilByteConsume (charToWord8 '"')
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
  


