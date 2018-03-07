{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}

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
import Data.Monoid
import GHC.Exts (Int#)
import GHC.Types
import Packed.Bytes (Bytes)
import Packed.Bytes.Small (ByteArray)
import GHC.Int (Int(I#))
import Data.Bits ((.&.),(.|.),unsafeShiftR)

import qualified Data.Char
import qualified Test.Tasty.Hedgehog as H
import qualified Packed.Text as T
import qualified Packed.Bytes.Small as BA
import qualified Packed.Bytes.Window as BAW
import qualified Packed.Bytes as B
import qualified Data.Set as S
import qualified GHC.OldList as L

main :: IO ()
main = do
  defaultMain tests

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
    , testProperty "decodeAscii" textDecodeAscii
    , testGroup "decodeUtf8"
      [ testProperty "isomorphism" textDecodeUtf8Iso
      , testProperty "surrogates" textDecodeUtf8Surrogates
        -- TODO: test against malformed inputs to decodeUtf8
      ]
    ]
  ]

isAscii :: Word8 -> Bool
isAscii w = w < 128

decodeAsciiList :: [Word8] -> Maybe String
decodeAsciiList = mapM
  (\w -> if isAscii w then Just (chr (fromIntegral w)) else Nothing)

textDecodeAscii :: Property
textDecodeAscii = property $ do
  bytes <- forAll genMostlyAsciiBytes
  front <- forAll (genOffset (L.length bytes))
  back <- forAll (genOffset (L.length bytes))
  let truncatedBytes = listDropEnd back (L.drop front bytes)
      expected = decodeAsciiList truncatedBytes
      actual = fmap T.unpack (T.decodeAscii (B.dropEnd back (B.drop front (B.pack bytes))))
  expected === actual

textDecodeUtf8Iso :: Property
textDecodeUtf8Iso = property $ do
  chars <- forAll genString
  front <- forAll (genOffset (L.length chars))
  back <- forAll (genOffset (L.length chars))
  let text = T.dropEnd back (T.drop front (T.pack chars))
      decoded = T.decodeUtf8 (T.encodeUtf8 text)
  Just (T.unpack text) === fmap T.unpack decoded

textDecodeUtf8Surrogates :: Property
textDecodeUtf8Surrogates = property $ do
  chars <- forAll genStringSurrogates
  front <- forAll (genOffset (L.length chars))
  back <- forAll (genOffset (L.length chars))
  let truncatedChars = listDropEnd back (L.drop front chars)
      cleanChars = map
        (\c -> if Data.Char.ord c >= 0xD800 && Data.Char.ord c < 0xE000 then chr 0xFFFD else c)
        truncatedChars
      bytes = B.pack (foldMap charToBytesWithSurrogates truncatedChars)
      text = T.dropEnd back (T.drop front (T.pack chars))
      decoded = T.decodeUtf8 (T.encodeUtf8 text)
  Just cleanChars === fmap T.unpack decoded
  Just (T.unpack text) === fmap T.unpack decoded
  Just cleanChars === fmap T.unpack (T.decodeUtf8 bytes)

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

genSurrogate :: Gen Char
genSurrogate = fmap chr (int (linear 0xD800 0xDFFF))

genSurrogates :: Gen String
genSurrogates = list (linear 0 3) genSurrogate

-- Generates a string that may contain unicode characters
-- in the range U+D800 to U+DFFF.
genStringSurrogates :: Gen String
genStringSurrogates = choice
  [ apcat [genSurrogates, genStringAscii, genSurrogates]
  , apcat [genSurrogates, genStringUnicode, genSurrogates]
  ]

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

genMostlyAsciiBytes :: Gen [Word8]
genMostlyAsciiBytes = choice
  [ apcat
    [ list (linear 0 20) (word8 (linear 0x00 0x7F))
    , fmap pure (word8 (linear 0xF0 0xFF))
    , list (linear 0 20) (word8 (linear 0x00 0x7F))
    ]
  , list (linear 0 45) (word8 (linear 0x00 0x7F))
  ]



findByteProp :: Property
findByteProp = property $ do
  wordList :: [Word8] <- forAll (list (linear 0 128) enumBounded)
  let len = L.length wordList
  mindex <- forAll $ frequency
    [ (4, fmap Just (int (linear 0 (len - 1))))
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

apcat :: (Applicative f, Monoid a) => [f a] -> f a
apcat = fmap mconcat . sequenceA

-- This is nearly the same this as Packed.Text.pack. However, it does
-- not replace surrogates with U+FFFD. This is useful for testing
-- that we handle surrogates correctly when decoding UTF-8 text.
charToBytesWithSurrogates :: Char -> [Word8]
charToBytesWithSurrogates c
  | p < 0x80 = [wordToWord8 p]
  | p < 0x800 = [wordToWord8 (byteTwoOne p), wordToWord8 (byteTwoTwo p)]
  | p < 0x10000 = [wordToWord8 (byteThreeOne p), wordToWord8 (byteThreeTwo p), wordToWord8 (byteThreeThree p)]
  | otherwise = [wordToWord8 (byteFourOne p), wordToWord8 (byteFourTwo p), wordToWord8 (byteFourThree p), wordToWord8 (byteFourFour p)]
  where
  p :: Word
  p = fromIntegral (Data.Char.ord c)
  
wordToWord8 :: Word -> Word8
wordToWord8 = fromIntegral

-- precondition: codepoint is less than 0x800
byteTwoOne :: Word -> Word
byteTwoOne w = unsafeShiftR w 6 .|. 0b11000000

byteTwoTwo :: Word -> Word
byteTwoTwo w = (w .&. 0b00111111) .|. 0b10000000

-- precondition: codepoint is less than 0x1000
byteThreeOne :: Word -> Word
byteThreeOne w = unsafeShiftR w 12 .|. 0b11100000

byteThreeTwo :: Word -> Word
byteThreeTwo w = (0b00111111 .&. unsafeShiftR w 6) .|. 0b10000000

byteThreeThree :: Word -> Word
byteThreeThree w = (w .&. 0b00111111) .|. 0b10000000

-- precondition: codepoint is less than 0x110000
byteFourOne :: Word -> Word
byteFourOne w = unsafeShiftR w 18 .|. 0b11110000

byteFourTwo :: Word -> Word
byteFourTwo w = (0b00111111 .&. unsafeShiftR w 12) .|. 0b10000000

byteFourThree :: Word -> Word
byteFourThree w = (0b00111111 .&. unsafeShiftR w 6) .|. 0b10000000

byteFourFour :: Word -> Word
byteFourFour w = (0b00111111 .&. w) .|. 0b10000000
-- let chars = ['\NUL']
-- let text = T.dropEnd 0 (T.drop 0 (T.pack chars))
    -- decoded = T.decodeUtf8 (T.encodeUtf8 text)
-- print (inspectBytes (T.encodeUtf8 text))
-- case BAW.findNonAscii' 0 2 (BA.pack [0b00101010, 0b11000000]) of
--   (# (# #) | #) -> putStrLn "non non-ascii characters"
--   (# | ix# #) -> putStrLn ("non-ascii character at " ++ show (I# ix#) )
-- case BAW.isUtf8 0 2 (BA.pack [0b00101010, 0b11111111]) of
--   (# _, (# ix | #) #) -> putStrLn $ "case one: " ++ show (I# ix)
--   (# _, (# | (# (# #) | #) #) #) -> putStrLn "case two"
--   _ -> putStrLn "case three"
-- case T.decodeAscii (B.pack [0]) of
--   Nothing -> putStrLn "definitely not ascii"
--   Just !_ -> putStrLn "definitely ascii"
-- case BAW.isUtf8 0 2 (BA.pack [0b11000000, 0b10000000]) of
--   (# _, (# ix | #) #) -> putStrLn $ "case one: " ++ show (I# ix)
--   (# _, (# | (# (# #) | #) #) #) -> putStrLn "case two"
--   _ -> putStrLn "case three"
-- case T.decodeResumeUtf8 (B.pack [0b11000000,0b10000000]) of
--   !_ -> putStrLn "at least it finished" 
  -- (# !_, Left !_ #) -> putStrLn "decoding failed somewhere"
  -- (# !_, Right !_ #) -> putStrLn "decoding was good"
  -- (# _, (# _ | #) #) -> putStrLn "decoding failed somewhere"
  -- _ -> putStrLn "decoding was good"
-- case T.decodeUtf8 (B.pack [0b00000000]) of
--   Nothing -> putStrLn "non text"
--   Just !_ -> putStrLn "was text"
-- print (Just (T.unpack text) == fmap T.unpack decoded)
-- print (baz 0 1)
-- putStrLn "made it past the worst"

-- inspectBytes :: Bytes -> (ByteArray,Int,Int)
-- inspectBytes (B.Bytes arr off len) = (arr,off,len)
-- 
-- type Maybe# (a :: TYPE (r :: RuntimeRep)) = (# (# #) | a #)
-- 
-- baz :: Int -> Int -> Char
-- baz start len = case fooBar start len of
--   (# (# #) | #) -> 'x' 
--   _ -> 'y'
-- 
-- fooBar :: 
--      Int -- start
--   -> Int -- len
--   -> Maybe# Int#
-- fooBar !start !len =
--   let !end0 = start + len
--    in if len < 8
--         then go start end0
--         else error "gountko"
--   where
--   go :: Int -> Int -> Maybe# Int#
--   go !ix !end = if ix < end
--     then case ix == 2 of
--       False -> go (ix + 1) end
--       True -> error "uheotnh" -- (# | unboxInt ix #)
--     else (# (# #) | #)
