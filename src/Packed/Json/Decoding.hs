module Packed.Json.Decoding
  ( JsonDecoding(..)
  ) where

import GHC.Exts (Any)

data JsonError
  = JsonErrorIndex !Int JsonError
  | JsonErrorKey !SmallText JsonError
  | JsonErrorMissing !SmallText
  | JsonErrorBase

data Value
  = ValueArray !(Array Value)
  | ValueObject () -- fix this, choose a map type
  | ValueNumber () -- fix this, choose a number type
  | ValueNull
  | ValueString !Text
  | ValueBool !Bool

data JsonDecoding :: Type -> Type where
  JsonDecodingRaw :: (Value -> Maybe a) -> JsonDecoding a
  JsonDecodingArray :: JsonDecoding a -> JsonDecoding (Array a)
  JsonDecodingPrimArray :: Prim a => JsonDecoding a -> JsonDecoding (PrimArray a)
  JsonDecodingCoerce :: Coercible a b => JsonDecoding b -> JsonDecoding a
  JsonDecodingInt :: JsonDecoding Int
  JsonDecodingMap :: FastMapDecoding a -> JsonDecoding a
  -- rethink map left fold
  JsonDecodingMapLeftFold :: (Text -> Maybe (k,JsonDecoding b)) -> (a -> k -> b -> b) -> a -> JsonDecoding a
  JsonDecodingArrayLeftFold :: JsonDecoding b -> (a -> b -> b) -> a -> JsonDecoding a
  JsonDecodingMapFix :: (d -> c -> c) -> (Text -> Maybe (c,Maybe (JsonDecoding b))) -> d -> (a -> c -> b -> b) -> JsonDecoding a

data MapDecoding f c a where
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

parseStreamST :: ByteStream s -> JsonDecoding a -> ST s (Either JsonError a)
parseStreamST = _


