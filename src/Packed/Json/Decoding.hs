{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Packed.Json.Decoding
  ( JsonDecoding(..)
  , valueParser
  ) where

import Data.Kind (Type)
import Data.Primitive (Array,PrimArray,Prim)
import Data.Type.Coercion (Coercion(..))
import Data.Word (Word8)
import GHC.Exts (Any)
import Packed.Bytes (Bytes(..))
import Packed.Bytes.Parser
import Packed.Bytes.Trie (Trie)
import Packed.Text (Text(..))
import Packed.Text.Small (SmallText)
import Data.Char (ord)
import Data.Coerce (coerce)

import qualified Data.Type.Coercion as C
import qualified Packed.Bytes.Parser as P

data JsonError
  = JsonErrorIndex !Int JsonError
  | JsonErrorKey !SmallText JsonError
  | JsonErrorMissing !SmallText
  | JsonErrorBase
  | FooBar

data Value
  = ValueArray !(Array Value)
  | ValueObject !(Array (Text,Value)) -- fix this, choose a better map type
  | ValueNumber !Word -- fix this, choose a better number type
  | ValueNull
  | ValueString !Text
  | ValueBool !Bool

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

decodingToParser :: JsonDecoding a -> Parser a
decodingToParser = \case
  JsonDecodingRaw f -> do
    v <- valueParser
    maybe P.failure pure (f v)
  JsonDecodingCoerce Coercion d -> coerce (decodingToParser d)
  JsonDecodingArray d -> do
    P.byte (charToWord8 '[')
    vals <- P.replicateIntersperseByte (charToWord8 ',') (decodingToParser d)
    P.byte (charToWord8 ']')
    pure vals 

valueParser :: Parser Value
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
    _ -> let w = word8ToWord x - 48
          in if w < 10
               then fmap ValueNumber (decimalWordStarting w)
               else P.failure
  P.skipSpace
  pure v
    
-- Fix this. It needs to validate that the bytes are UTF-8 encoded
-- text.
stringParserAfterQuote :: Parser Text
stringParserAfterQuote = do
  Bytes arr off len <- takeBytesUntilByteConsume (charToWord8 '"')
  pure (Text arr (fromIntegral off) len)


charToWord8 :: Char -> Word8
charToWord8 = fromIntegral . ord

word8ToWord :: Word8 -> Word
word8ToWord = fromIntegral

