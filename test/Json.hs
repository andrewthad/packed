{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}

module Json
  ( valueParserProperty
  , contextA
  , objectA
  , objectB
  , objectC
  , objectD
  , objectE
  ) where

import Control.Applicative
import Control.Monad.ST (ST,runST)
import Data.Primitive (Array)
import Data.Word (Word8)
import Hedgehog (Property,Gen,property,forAll,(===),failure)
import Hedgehog.Gen (list,enumBounded,int,frequency,choice,element,integral,word8,word)
import Hedgehog.Range (Range,linear)
import Packed.Bytes (Bytes)
import Packed.Bytes.Stream.Parser (Parser)
import Packed.Bytes.Set (ByteSet)
import Packed.Bytes.Small (ByteArray)
import Packed.Bytes.Stream.ST (ByteStream)
import Packed.Bytes.Trie (Trie)
import Data.Map (Map)
import Packed.Json.Decoding (Value(..))
import Packed.Text (Text)
import Test.Tasty.HUnit (assertEqual,Assertion)
import qualified Data.List.Split as LS
import qualified Data.Char
import qualified Data.Map.Strict as M
import qualified GHC.OldList as L
import qualified Packed.Bytes as B
import qualified Packed.Bytes.Stream.Parser as P
import qualified Packed.Bytes.Set as ByteSet
import qualified Packed.Bytes.Stream.ST as Stream
import qualified Packed.Bytes.Trie as Trie
import qualified Data.Primitive as PM
import qualified Packed.Json.Decoding as JD

contextA :: Assertion
contextA = do
  let sample = L.concat
        [ "["
        ,   "{\"name\":\"drew\"},"
        ,   "{\"name\":null, \"ages\": [13,18,29,badinput,71],"
        ,   "{\"name\":\"jordan\"}"
        , "]"
        ]
  assertEqual "context"
    ( Left
      $ JD.JsonErrorIndex 1
      $ JD.JsonErrorKey "ages"
      $ JD.JsonErrorIndex 3
      $ JD.JsonErrorCause (JD.ErrorInvalidTokenHead 98)
    )
    (JD.decode (JD.fromJson @JD.Value) (B.pack (map charToWord8 sample)))

objectA :: Property
objectA = property $ do
  let sample = " {\"person_name\" : \"bob\" , \"person_living\":false,\"person_age\"  :12}"
  elementsPerChunk <- forAll $ int (linear 1 (L.length sample))
  let strChunks = LS.chunksOf elementsPerChunk sample
      chunks = map (B.pack . map charToWord8) strChunks
      r = runST (flattenLeftovers $ JD.decodeStreamST JD.fromJson (foldMap Stream.fromBytes chunks))
  Right (Person "bob" 12 False) === r

objectB :: Property
objectB = property $ do
  let sample = " { \"number\" : 58 } "
  elementsPerChunk <- forAll $ int (linear 1 (L.length sample))
  let strChunks = LS.chunksOf elementsPerChunk sample
      chunks = map (B.pack . map charToWord8) strChunks
      r = runST (flattenLeftovers $ JD.decodeStreamST (JD.object (JD.key "number" JD.fromJson Nothing)) (foldMap Stream.fromBytes chunks))
  Right (58 :: Word) === r

objectC :: Property
objectC = property $ do
  let sample = "{   }"
  elementsPerChunk <- forAll $ int (linear 1 (L.length sample))
  let strChunks = LS.chunksOf elementsPerChunk sample
      chunks = map (B.pack . map charToWord8) strChunks
      r = runST (flattenLeftovers $ JD.decodeStreamST (JD.object (JD.key "number" JD.fromJson (Just 47))) (foldMap Stream.fromBytes chunks))
  Right (47 :: Word) === r

objectD :: Property
objectD = property $ do
  let sample = " { \"second\" : false, \"first\" : true } "
  elementsPerChunk <- forAll $ int (linear 1 (L.length sample))
  let strChunks = LS.chunksOf elementsPerChunk sample
      chunks = map (B.pack . map charToWord8) strChunks
      r = runST
        (flattenLeftovers $ JD.decodeStreamST
          (JD.object $ (,)
            <$> JD.key "first" JD.fromJson Nothing
            <*> JD.key "second" JD.fromJson Nothing
          )
          (foldMap Stream.fromBytes chunks)
        )
  Right (True,False) === r

objectE :: Property
objectE = property $ do
  let sample = " { \"altruism\" : 12, \"alto\" : 15, \"alexa\": 7 } "
  elementsPerChunk <- forAll $ int (linear 1 (L.length sample))
  let strChunks = LS.chunksOf elementsPerChunk sample
      chunks = map (B.pack . map charToWord8) strChunks
      r = runST
        (flattenLeftovers $ JD.decodeStreamST
          (JD.object $ (,,)
            <$> JD.key "altruism" JD.fromJson Nothing
            <*> JD.key "alexa" JD.fromJson Nothing
            <*> JD.key "alto" JD.fromJson Nothing
          )
          (foldMap Stream.fromBytes chunks)
        )
  Right (12 :: Word,7 :: Word,15 :: Word) === r

valueParserProperty :: Property
valueParserProperty = property $ do
  let sample = "[ 55, 24.06, 19E3,-16e-2,18.1e4, \"hello\", [ true, null, false ]]"
  elementsPerChunk <- forAll $ int (linear 1 (L.length sample))
  let strChunks = LS.chunksOf elementsPerChunk sample
      chunks = map (B.pack . map charToWord8) strChunks
      stream = foldMap Stream.fromBytes chunks
      (r,mextra) = runExampleParser (JD.valueParser <* P.endOfInput) stream
  Nothing === mextra
  Just
    ( ValueArray
      [ ValueNumber 55
      , ValueNumber 24.06
      , ValueNumber 19000
      , ValueNumber (-0.16)
      , ValueNumber 181000
      , ValueString "hello"
      , ValueArray [ ValueBool True, ValueNull, ValueBool False ]
      ]
    ) === r

data Person = Person
  { personName :: !Text
  , personAge :: !Word
  , personLiving :: !Bool
  } deriving (Eq,Show)

instance JD.FromJson Person where
  fromJson = JD.object $ pure Person
    <*> JD.key "person_name" JD.fromJson Nothing 
    <*> JD.key "person_age" JD.fromJson (Just 42) 
    <*> JD.key "person_living" JD.fromJson Nothing 

runExampleParser :: (JD.Context -> Parser JD.ContextualizedError a) -> (forall s. ByteStream s) -> (Maybe a, Maybe String)
runExampleParser parser stream = runST $ do
  P.Result mleftovers r <- P.parseStreamST stream (parser JD.ContextNil)
  mextra <- case mleftovers of
    Nothing -> return Nothing
    Just (P.Leftovers chunk remainingStream) -> do
      bs <- Stream.unpack remainingStream
      return (Just (map word8ToChar (B.unpack chunk ++ bs)))
  return (either (const Nothing) Just r,mextra)

flattenLeftovers ::
     ST s (Either (JD.JsonError,Maybe (P.Leftovers s)) a)
  -> ST s (Either (JD.JsonError,String) a)
flattenLeftovers x = do
  y <- x
  case y of
    Right a -> return (Right a)
    Left (err,mleftovers) -> case mleftovers of
      Nothing -> return (Left (err,""))
      Just (P.Leftovers bs leftovers) -> do
        cs <- Stream.unpack leftovers
        return (Left (err,map word8ToChar (B.unpack bs) ++ map word8ToChar cs))

charToWord8 :: Char -> Word8
charToWord8 = fromIntegral . Data.Char.ord

word8ToChar :: Word8 -> Char
word8ToChar = Data.Char.chr . fromIntegral

