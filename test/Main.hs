{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

import Data.Set (Set)
import Data.Word (Word8)
import Hedgehog (property,forAll,Property,(===),failure)
import Hedgehog.Gen (list,enumBounded,int,frequency)
import Hedgehog.Range (Range,linear)
import Test.Tasty (defaultMain,testGroup,TestTree)
import Data.Bits ((.&.))

import qualified Test.Tasty.Hedgehog as H
import qualified Byte.Array as BA
import qualified Data.Set as S
import qualified GHC.OldList as L

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ testGroup "ByteArray"
    [ H.testProperty "findByte" findByteProp
    , H.testProperty "zipAnd" zipAndProp
    ]
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

