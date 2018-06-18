{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}

module Json
  ( valueParserProperty
  ) where

import Control.Applicative
import Control.Monad.ST (ST,runST)
import Data.Primitive (Array)
import Data.Word (Word8)
import Hedgehog (Property,Gen,property,forAll,(===),failure)
import Hedgehog.Gen (list,enumBounded,int,frequency,choice,element,integral,word8,word)
import Hedgehog.Range (Range,linear)
import Packed.Bytes (Bytes)
import Packed.Bytes.Parser (Parser)
import Packed.Bytes.Set (ByteSet)
import Packed.Bytes.Small (ByteArray)
import Packed.Bytes.Stream.ST (ByteStream)
import Packed.Bytes.Trie (Trie)
import Data.Map (Map)
import Packed.Json.Decoding (Value(..))
import qualified Data.List.Split as LS
import qualified Data.Char
import qualified Data.Map.Strict as M
import qualified GHC.OldList as L
import qualified Packed.Bytes as B
import qualified Packed.Bytes.Parser as P
import qualified Packed.Bytes.Set as ByteSet
import qualified Packed.Bytes.Stream.ST as Stream
import qualified Packed.Bytes.Trie as Trie
import qualified Data.Primitive as PM
import qualified Packed.Json.Decoding as JD

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


runExampleParser :: Parser e () a -> (forall s. ByteStream s) -> (Maybe a, Maybe String)
runExampleParser parser stream = runST $ do
  P.Result mleftovers r _ <- P.parseStreamST stream () parser
  mextra <- case mleftovers of
    Nothing -> return Nothing
    Just (P.Leftovers chunk remainingStream) -> do
      bs <- Stream.unpack remainingStream
      return (Just (map word8ToChar (B.unpack chunk ++ bs)))
  return (either (const Nothing) Just r,mextra)

charToWord8 :: Char -> Word8
charToWord8 = fromIntegral . Data.Char.ord

word8ToChar :: Word8 -> Char
word8ToChar = Data.Char.chr . fromIntegral

