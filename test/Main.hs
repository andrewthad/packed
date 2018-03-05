{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

import Data.Set (Set)
import Data.Word (Word8)
import Hedgehog (Property,Gen,property,forAll,(===),failure)
import Hedgehog.Gen (list,enumBounded,int,frequency,choice,element,integral,word8)
import Hedgehog.Range (Range,linear)
import Test.Tasty (defaultMain,testGroup,TestTree)
import Data.Bits ((.&.))
import Data.Char (chr)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.Hedgehog (testProperty)
import Data.Bifunctor (bimap)

import qualified Data.Char
import qualified Test.Tasty.Hedgehog as H
import qualified Text.Slice as T
import qualified Byte.Array as BA
import qualified Byte.Slice as B
import qualified Data.Set as S
import qualified GHC.OldList as L

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ testGroup "ByteArray"
    [ testProperty "findByte" findByteProp
    , testProperty "zipAnd" zipAndProp
    ]
  , testGroup "Bytes"
    [ testProperty "findByte" sliceFindByteProp
    ]
  , testGroup "Text"
    [ testProperty "pack" textPackProp
    , testProperty "take" textTakeProp
    , testProperty "drop" textDropProp
    , testProperty "breakOnChar" textBreakCharProp
    , testProperty "toUpper" textToUpperProp
    ]
  ]

-- textPackU80 :: Assertion
-- textPackU80 = T.pack

textPackProp :: Property
textPackProp = property $ do
  chars <- forAll genString
  front <- forAll (genOffset (L.length chars))
  back <- forAll (genOffset (L.length chars))
  let expected = listDropEnd back (L.drop front chars)
      actual = T.unpack (T.dropEnd back (T.drop front (T.pack chars)))
  expected === actual

textTakeProp :: Property
textTakeProp = property $ do
  chars <- forAll genString
  n <- forAll (genChop (L.length chars))
  let expected = L.take n chars
      actual = T.unpack (T.take n (T.pack chars))
  expected === actual

textDropProp :: Property
textDropProp = property $ do
  chars <- forAll genString
  n <- forAll (genChop (L.length chars))
  let expected = L.drop n chars
      actual = T.unpack (T.drop n (T.pack chars))
  expected === actual


textBreakCharProp :: Property
textBreakCharProp = property $ do
  chars <- forAll genString
  front <- forAll (genOffset (L.length chars))
  back <- forAll (genOffset (L.length chars))
  let truncatedChars = listDropEnd back (L.drop front chars)
  c <- forAll (pickChar truncatedChars)
  let expected = L.break (== c) truncatedChars
      actual = bimap T.unpack T.unpack (T.breakChar c (T.dropEnd back (T.drop front (T.pack chars))))
  expected === actual

textToUpperProp :: Property
textToUpperProp = property $ do
  chars <- forAll genString
  front <- forAll (genOffset (L.length chars))
  back <- forAll (genOffset (L.length chars))
  let expected = L.map Data.Char.toUpper (listDropEnd back (L.drop front chars))
      actual = T.unpack (T.toUpper (T.dropEnd back (T.drop front (T.pack chars))))
  expected === actual

listDropEnd :: Int -> [a] -> [a]
listDropEnd n xs = L.take (L.length xs - n) xs

pickChar :: String -> Gen Char
pickChar s = if L.null s
  then genCharUnicode
  else element s

pickByte :: [Word8] -> Gen Word8
pickByte s = if L.null s
  then genByte
  else element s

genChop :: Int -> Gen Int
genChop originalLen = integral (linear 0 maxDiscard)
  where
  maxDiscard = div (originalLen * 6) 5

genOffset :: Int -> Gen Int
genOffset originalLen = integral (linear 0 maxDiscard)
  where
  maxDiscard = min 19 (div originalLen 3)

-- Generates a string that is either entirely ascii
-- or that is a healthy mixture of characters with
-- variable UTF-8 byte lengths.
genString :: Gen String
genString = frequency [ (3, genStringAscii), (7, genStringUnicode) ]

-- Only uses ascii characters
genStringAscii :: Gen String
genStringAscii = list (linear 0 128) (fmap chr (int (linear 0x00 0x7F)))

-- Pulls from unicode characters that have all different
-- UTF-8 byte-lengths. 
genStringUnicode :: Gen String
genStringUnicode = list (linear 0 128) genCharUnicode

genCharUnicode :: Gen Char
genCharUnicode = choice
  [ fmap chr (int (linear 0x00 0x7F))
  , fmap chr (int (linear 0x80 0x7FF))
  , fmap (chr . (\x -> if x >= 0xD800 && x <= 0xDFFF then 0xD799 else x)) (int (linear 0x800 0xFFFF))
  , fmap chr (int (linear 0x10000 0x10FFFF))
  ]

genByte :: Gen Word8
genByte = word8 (linear minBound maxBound)


findByteProp :: Property
findByteProp = property $ do
  wordList :: [Word8] <- forAll (list (linear 0 128) enumBounded)
  let len = L.length wordList
  mindex <- forAll $ frequency
    [ (4, fmap Just (int (linear 0 len)))
    , (1, pure Nothing)
    ]
  w <- case mindex of
    Just ix -> case safeIndex ix wordList of
      Just b -> pure b
      Nothing -> if len == 0 && ix == 0 then pure 0 else failure
    Nothing -> case findUnusedByte (S.fromList wordList) of
      Just b -> pure b
      Nothing -> failure
  L.elemIndex w wordList === BA.findByte w (BA.pack wordList)

sliceFindByteProp :: Property
sliceFindByteProp = property $ do
  byteList <- forAll (list (linear 0 128) genByte)
  front <- forAll (genOffset (L.length byteList))
  back <- forAll (genOffset (L.length byteList))
  let truncatedByteList = listDropEnd back (L.drop front byteList)
  w <- forAll (pickByte truncatedByteList)
  let expected = L.elemIndex w truncatedByteList
      actual = B.findByte w (B.dropEnd back (B.drop front (B.pack byteList)))
  expected === actual


zipAndProp :: Property
zipAndProp = property $ do
  xsList :: [Word8] <- forAll (list (linear 0 128) enumBounded)
  ysList :: [Word8] <- forAll (list (linear 0 128) enumBounded)
  let xs = BA.pack xsList
      ys = BA.pack ysList
  L.zipWith (.&.) xsList ysList === BA.unpack (BA.zipAnd xs ys)

safeIndex :: Int -> [a] -> Maybe a
safeIndex !_ [] = Nothing
safeIndex !ix (x : xs) = case compare ix 0 of
  EQ -> Just x
  LT -> Nothing
  GT -> safeIndex (ix - 1) xs

findUnusedByte :: Set Word8 -> Maybe Word8
findUnusedByte s = S.lookupMin (S.difference allBytes s)

allBytes :: Set Word8
allBytes = S.fromList (enumFromTo minBound maxBound)

