{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

import Data.Set (Set)
import Data.Word (Word8)
import Hedgehog (Property,Gen,property,forAll,(===),failure)
import Hedgehog.Gen (list,enumBounded,int,frequency,choice,element,integral)
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
  chars === T.unpack (T.pack chars)

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
  c <- forAll (pickChar chars)
  let expected = L.break (== c) chars
      actual = bimap T.unpack T.unpack (T.breakChar c (T.pack chars))
  expected === actual

textToUpperProp :: Property
textToUpperProp = property $ do
  chars <- forAll genString
  -- frontChop <- forAll (genOffset (L.length chars))
  -- backChop <- forAll (genOffset (L.length chars))
  let expected = L.map Data.Char.toUpper chars
      actual = T.unpack (T.toUpper (T.pack chars))
  expected === actual

pickChar :: String -> Gen Char
pickChar s = if L.null s
  then genCharUnicode
  else element s

genChop :: Int -> Gen Int
genChop originalLen = integral (linear 0 maxDiscard)
  where
  maxDiscard = div (originalLen * 6) 5

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

