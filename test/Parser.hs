{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}

module Parser
  ( byteParserArtificalA
  , byteParserArtificalB
  , byteParserArtificalDelta
  , byteParserHttpRequest
  , byteParserDecimalWord
  , byteParserEolAccept
  , byteParserEolReject
  , byteParserTrieSnmp
  , byteParserTrieNumbers
  ) where

import Control.Applicative
import Control.Monad.ST (ST,runST)
import Data.Primitive (Array)
import Data.Word (Word8)
import GHC.Exts (fromList,unsafeCoerce#)
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
import qualified Data.List.Split as LS
import qualified Data.Char
import qualified Data.Map.Strict as M
import qualified GHC.OldList as L
import qualified Packed.Bytes as B
import qualified Packed.Bytes.Parser as P
import qualified Packed.Bytes.Set as ByteSet
import qualified Packed.Bytes.Stream.ST as Stream
import qualified Packed.Bytes.Trie as Trie

-- from common directory
import qualified Parser.Http.Request as Request
import qualified Data.Trie.Naive as Naive
import qualified MinimalTrieParser as MTP

byteParserDecimalWord :: Property
byteParserDecimalWord = property $ do
  w <- forAll (word (linear minBound maxBound))
  let stream = foldMap (Stream.singleton . charToWord8) (show w)
  let v = runST $ do
        P.Result Nothing (Just x) <- P.parseStreamST stream P.decimalWord
        return x
  w === v

data ArtificialAlpha = ArtificialAlpha
  { artificialAlphaNumber :: !Word
  , artificialAlphaLetter :: !Char
  } deriving (Eq,Show)

byteParserArtificalA :: Property
byteParserArtificalA = property $ do
  let sample = "With 524, 0xFABAC1D9   choice C is the answer."
  elementsPerChunk <- forAll $ int (linear 1 (L.length sample))
  let strChunks = LS.chunksOf elementsPerChunk sample
      chunks = map (B.pack . map charToWord8) strChunks
      stream = foldMap Stream.fromBytes chunks
      (r,mextra) = runExampleParser parserArtificialA stream
  Nothing === mextra
  Just (ArtificialAlpha 524 'C') === r

parserArtificialA :: Parser ArtificialAlpha
parserArtificialA = do
  P.bytes (B.pack (map charToWord8 "With "))
  n <- P.decimalWord
  P.byte (charToWord8 ',')
  P.skipSpace
  P.byte (charToWord8 '0')
  P.byte (charToWord8 'x')
  _ <- P.takeBytesWhileMember hexSet
  P.skipSpace
  P.bytes (B.pack (map charToWord8 "choice "))
  c <- P.any
  P.bytes (B.pack (map charToWord8 " is the answer."))
  P.endOfInput
  return (ArtificialAlpha n (word8ToChar c))

data ArtificialBeta = ArtificialBeta
  { artificialBetaName :: !Bytes
  , artificialBetaAttributes :: !(Array Bytes)
  } deriving (Eq,Show)

byteParserArtificalB :: Property
byteParserArtificalB = property $ do
  let sample = "Name: Drew. Attributes: 3 (fire,water,ice,)."
  elementsPerChunk <- forAll $ int (linear 1 (L.length sample))
  let strChunks = LS.chunksOf elementsPerChunk sample
      chunks = map (B.pack . map charToWord8) strChunks
      stream = foldMap Stream.fromBytes chunks
      (r,mextra) = runExampleParser parserArtificialB stream
      expected = fromList [s2b "fire", s2b "water", s2b "ice"]
  Nothing === mextra
  Just (ArtificialBeta (s2b "Drew") expected) === r

parserArtificialB :: Parser ArtificialBeta
parserArtificialB = do
  P.bytes (s2b "Name: ")
  name <- P.takeBytesUntilByteConsume (c2w '.')
  P.bytes (s2b " Attributes: ")
  attrCount <- P.decimalWord
  P.bytes (s2b " (")
  attrs <- P.replicate (wordToInt attrCount) (P.takeBytesUntilByteConsume (c2w ','))
  P.bytes (s2b ").")
  P.endOfInput
  return (ArtificialBeta name attrs)

byteParserArtificalDelta :: Property
byteParserArtificalDelta = property $ do
  let sample = "56437:2145:123:24:2"
  elementsPerChunk <- forAll $ int (linear 1 (L.length sample))
  let strChunks = LS.chunksOf elementsPerChunk sample
      chunks = map (B.pack . map charToWord8) strChunks
      stream = foldMap Stream.fromBytes chunks
      (r,mextra) = runExampleParser parserArtificialDelta stream
  Nothing === mextra
  Just () === r

byteParserTrieSnmp :: Property
byteParserTrieSnmp = property $ do
  let sample = "STRING: _55_ INTEGER: 12 OID: 13,16 Timeticks: Hello "
  elementsPerChunk <- forAll $ int (linear 1 (L.length sample))
  let strChunks = LS.chunksOf elementsPerChunk sample
      chunks = map (B.pack . map charToWord8) strChunks
      stream = foldMap Stream.fromBytes chunks
      expected = fromList [55, 12, 29, 6] :: Array Word
      (r,mextra) = runExampleParser (P.replicateUntilEnd (P.trie snmptrapd)) stream
  Nothing === mextra
  Just expected === r

snmptrapd :: Trie.Trie (Parser Word)
snmptrapd = Trie.fromList snmptradpPairs

snmptradpPairs :: [(Bytes,Parser Word)]
snmptradpPairs =
  [ (s2b "STRING: ", P.byte (c2w '_') *> P.decimalWord <* P.byte (c2w '_') <* P.byte (c2w ' '))
  , (s2b "INTEGER: ", P.decimalWord <* P.skipSpace)
  , (s2b "OID: ", liftA2 (+) (P.decimalWord <* P.byte (c2w ',')) P.decimalWord <* P.skipSpace)
  , (s2b "Timeticks: ", 6 <$ P.skipUntilByteConsume (c2w ' '))
  ]

{-# NOINLINE altMap #-}
altMap :: Map Word8 (Bytes,Parser Word)
altMap = M.fromList
  [(c2w 'S',(s2b "trauss",P.decimalWord <* P.byte (c2w ' ')))
  ,(c2w 'J',(s2b "hey", P.failure))
  ]

{-# NOINLINE useMap #-}
useMap :: Parser Word
useMap = do
  w <- P.any
  case M.lookup w altMap of
    Nothing -> P.failure
    Just (x,p) -> P.bytes x >> p

byteParserTrieNumbers :: Property
byteParserTrieNumbers = property $ do
  let sample = "five one nine six seven two"
  elementsPerChunk <- forAll $ int (linear 1 (L.length sample))
  let strChunks = LS.chunksOf elementsPerChunk sample
      chunks = map (B.pack . map charToWord8) strChunks
      stream = foldMap Stream.fromBytes chunks
      expected = fromList [5, 1, 9, 6, 7, 2] :: Array Word
      (r,mextra) = runExampleParser (P.replicateIntersperseByte (c2w ' ') (P.triePure numberTrie) <* P.endOfInput) stream
  Nothing === mextra
  Just expected === r

numberTrie :: Trie.Trie Word
numberTrie = Trie.fromList
  [ (s2b "one", 1), (s2b "two", 2), (s2b "three", 3)
  , (s2b "four", 4), (s2b "five", 5), (s2b "six", 6)
  , (s2b "seven", 7), (s2b "eight", 8), (s2b "nine", 9)
  , (s2b "zero", 0)
  ]

parserArtificialDelta :: Parser ()
parserArtificialDelta = do
  P.skipDigits
  name <- P.byte (c2w ':')
  P.skipDigits
  name <- P.byte (c2w ':')
  P.skipDigits
  name <- P.byte (c2w ':')
  P.skipDigits
  name <- P.byte (c2w ':')
  P.skipDigits
  P.endOfInput

byteParserEolAccept :: Property
byteParserEolAccept = property $ do
  let sample = "age\nof\r\nthe\ngreatest\r\nmusic\n\r\ntoday\n"
  elementsPerChunk <- forAll $ int (linear 1 (L.length sample))
  let strChunks = LS.chunksOf elementsPerChunk sample
      chunks = map (B.pack . map charToWord8) strChunks
      stream = foldMap Stream.fromBytes chunks
      (r,mextra) = runExampleParser
        (P.replicateUntilEnd P.takeBytesUntilEndOfLineConsume)
        stream
      expected = fromList
        [ s2b "age", s2b "of", s2b "the", s2b "greatest"
        , s2b "music", s2b "", s2b "today"
        ] :: Array Bytes
  Nothing === mextra
  Just expected === r

byteParserEolReject :: Property
byteParserEolReject = property $ do
  let sample = "the\nemporium\rhas\narrived"
  elementsPerChunk <- forAll $ int (linear 1 (L.length sample))
  let strChunks = LS.chunksOf elementsPerChunk sample
      chunks = map (B.pack . map charToWord8) strChunks
      stream = foldMap Stream.fromBytes chunks
      (r,mextra) = runExampleParser
        (P.replicateUntilEnd P.takeBytesUntilEndOfLineConsume)
        stream
  Just "\rhas\narrived" === mextra

byteParserHttpRequest :: Property
byteParserHttpRequest = property $ do
  elementsPerChunk <- forAll $ int (linear 1 (L.length Request.sample))
  let strChunks = LS.chunksOf elementsPerChunk Request.sample
      chunks = map (B.pack . map charToWord8) strChunks
      stream = foldMap Stream.fromBytes chunks
      (r,mextra) = runExampleParser Request.parser stream
  Nothing === mextra
  Just Request.expected === r

runExampleParser :: Parser a -> (forall s. ByteStream s) -> (Maybe a, Maybe String)
runExampleParser parser stream = runST $ do
  P.Result mleftovers r <- P.parseStreamST stream parser
  mextra <- case mleftovers of
    Nothing -> return Nothing
    Just (P.Leftovers chunk remainingStream) -> do
      bs <- Stream.unpack remainingStream
      return (Just (map word8ToChar (B.unpack chunk ++ bs)))
  return (r,mextra)

s2b :: String -> Bytes
s2b = B.pack . map charToWord8

c2w :: Char -> Word8
c2w = charToWord8

hexSet :: ByteSet
hexSet = ByteSet.fromList (map charToWord8 (concat [['a'..'f'],['A'..'F'],['0'..'9']]))

charToWord8 :: Char -> Word8
charToWord8 = fromIntegral . Data.Char.ord

word8ToChar :: Word8 -> Char
word8ToChar = Data.Char.chr . fromIntegral

wordToInt :: Word -> Int
wordToInt = fromIntegral

