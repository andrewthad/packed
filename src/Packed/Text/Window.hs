{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}

{-# OPTIONS_GHC
 -Weverything
 -fno-warn-unsafe
 -fno-warn-implicit-prelude
 -fno-warn-missing-import-lists
 -O2
#-}

module Packed.Text.Window
  ( unpack
  , pack
  ) where

import Data.Char (ord,chr)
import Packed.Bytes.Small (ByteArray)
import GHC.Exts (Word#)
import GHC.Int (Int(I#))
import GHC.Word (Word(W#),Word8(W8#))
import Data.Bits ((.&.),(.|.),unsafeShiftR,unsafeShiftL,complement)
import Control.Monad.ST (ST,runST)
import qualified Data.Primitive as PM
import qualified Packed.Bytes.Small as BA
import qualified Packed.Bytes.Window as BAW

unpack :: Int -> Int -> ByteArray -> String
unpack !off !len !arr = go off
  where
  go :: Int -> String
  go !ix0 = if ix0 < len + off
    then
      let !(!ix1,!c) = nextChar arr ix0
       in c : go ix1
    else []

nextCharIx :: ByteArray -> Int -> Int
nextCharIx !arr !ix
  | oneByteChar firstByte = ix + 1
  | twoByteChar firstByte = ix + 2
  | threeByteChar firstByte = ix + 3
  | otherwise = ix + 4
  where
  firstByte :: Word8
  !firstByte = BA.unsafeIndex arr ix

oneByteChar :: Word8 -> Bool
oneByteChar w = w .&. 0b10000000 == 0

twoByteChar :: Word8 -> Bool
twoByteChar w = w .&. 0b11100000 == 0b11000000

threeByteChar :: Word8 -> Bool
threeByteChar w = w .&. 0b11110000 == 0b11100000

nextChar :: ByteArray -> Int -> (Int,Char)
nextChar !arr !ix
  | oneByteChar firstByte = (ix + 1, wordToChar (word8ToWord firstByte))
  | twoByteChar firstByte =
      let !secondByte = BA.unsafeIndex arr (ix + 1)
       in (ix + 2, charFromTwoBytes firstByte secondByte)
  | threeByteChar firstByte =
      let !secondByte = BA.unsafeIndex arr (ix + 1)
          !thirdByte = BA.unsafeIndex arr (ix + 2)
       in (ix + 3, charFromThreeBytes firstByte secondByte thirdByte)
  | otherwise =
      let !secondByte = BA.unsafeIndex arr (ix + 1)
          !thirdByte = BA.unsafeIndex arr (ix + 2)
          !fourthByte = BA.unsafeIndex arr (ix + 3)
       in (ix + 4, charFromFourBytes firstByte secondByte thirdByte fourthByte)
  where
  firstByte :: Word8
  !firstByte = BA.unsafeIndex arr ix

charFromTwoBytes :: Word8 -> Word8 -> Char
charFromTwoBytes w1 w2 = wordToChar $
  unsafeShiftL (word8ToWord w1 .&. 0b00011111) 6 .|. 
  (word8ToWord w2 .&. 0b00111111)

charFromThreeBytes :: Word8 -> Word8 -> Word8 -> Char
charFromThreeBytes w1 w2 w3 = wordToChar $
  unsafeShiftL (word8ToWord w1 .&. 0b00001111) 12 .|. 
  unsafeShiftL (word8ToWord w2 .&. 0b00111111) 6 .|. 
  (word8ToWord w3 .&. 0b00111111)

charFromFourBytes :: Word8 -> Word8 -> Word8 -> Word8 -> Char
charFromFourBytes w1 w2 w3 w4 = wordToChar $
  unsafeShiftL (word8ToWord w1 .&. 0b00000111) 18 .|. 
  unsafeShiftL (word8ToWord w2 .&. 0b00111111) 12 .|. 
  unsafeShiftL (word8ToWord w3 .&. 0b00111111) 6 .|. 
  (word8ToWord w4 .&. 0b00111111)

word8ToWord :: Word8 -> Word
word8ToWord = fromIntegral

wordToChar :: Word -> Char
wordToChar w = chr (fromIntegral w)

-- The result contains the byte array of UTF8-encoded text and
-- a word indicating the multiplicity (0 if there were only characters
-- from the ascii plane and 0b1000... if there were non-ascii characters).
pack :: String -> (ByteArray,Word)
pack str = case metadata 0 single str of
  (!totalBytes,!totalMult) -> 
    let !arr = runST $ do
          marr <- PM.newByteArray totalBytes
          let go [] !_ = return ()
              go (!c : cs) !ix0 = do
                ix1 <- writeChar c ix0 marr
                go cs ix1
          go str 0
          PM.unsafeFreezeByteArray marr
     in (arr,totalMult)
  where
  metadata :: Int -> Word -> [Char] -> (Int,Word)
  metadata !totalBytes !totalMult [] = (totalBytes,totalMult)
  metadata !totalBytes !totalMult (!c : cs) =
    let !bytes = charBytes c
        !mult = if bytes < 2 then single else multiple
     in metadata (bytes + totalBytes) (appendMult mult totalMult) cs

appendMult :: Word -> Word -> Word
appendMult = (.|.)

single :: Word
single = 0

multiple :: Word
multiple = binaryOneThenZeroes

binaryOneThenZeroes :: Word
binaryOneThenZeroes = maxBound - div (maxBound :: Word) 2

-- Result is between 1 and 4. The guards used here do not have to treat
-- surrogates as a special case.
charBytes :: Char -> Int
charBytes !c
  | codepoint < 0x80 = 1
  | codepoint < 0x800 = 2
  | codepoint < 0x10000 = 3
  | otherwise = 4
  where
  !codepoint = intToWord (ord c)

intToWord :: Int -> Word
intToWord = fromIntegral

-- returns the new index
writeChar :: Char -> Int -> PM.MutableByteArray s -> ST s Int
writeChar !c !ix !marr
  | codepoint < 0x80 = do
      PM.writeByteArray marr ix (unsafeWordToWord8 codepoint)
      return (ix + 1)
  | codepoint < 0x800 = do
      PM.writeByteArray marr ix (unsafeWordToWord8 (byteTwoOne codepoint))
      PM.writeByteArray marr (ix + 1) (unsafeWordToWord8 (byteTwoTwo codepoint))
      return (ix + 2)
  | surrogate codepoint = do
      -- Codepoint U+FFFD
      PM.writeByteArray marr ix (0xEF :: Word8)
      PM.writeByteArray marr (ix + 1) (0xBF :: Word8)
      PM.writeByteArray marr (ix + 2) (0xBD :: Word8)
      return (ix + 3)
  | codepoint < 0x10000 = do
      PM.writeByteArray marr ix (unsafeWordToWord8 (byteThreeOne codepoint))
      PM.writeByteArray marr (ix + 1) (unsafeWordToWord8 (byteThreeTwo codepoint))
      PM.writeByteArray marr (ix + 2) (unsafeWordToWord8 (byteThreeThree codepoint))
      return (ix + 3)
  | otherwise = do
      PM.writeByteArray marr ix (unsafeWordToWord8 (byteFourOne codepoint))
      PM.writeByteArray marr (ix + 1) (unsafeWordToWord8 (byteFourTwo codepoint))
      PM.writeByteArray marr (ix + 2) (unsafeWordToWord8 (byteFourThree codepoint))
      PM.writeByteArray marr (ix + 3) (unsafeWordToWord8 (byteFourFour codepoint))
      return (ix + 4)
  where
  !codepoint = intToWord (ord c)

unsafeWordToWord8 :: Word -> Word8
unsafeWordToWord8 (W# w) = W8# w

-- precondition: codepoint is less than 0x110000
byteFourOne :: Word -> Word
byteFourOne w = unsafeShiftR w 18 .|. 0b11110000

byteFourTwo :: Word -> Word
byteFourTwo w = (0b00111111 .&. unsafeShiftR w 12) .|. 0b10000000

byteFourThree :: Word -> Word
byteFourThree w = (0b00111111 .&. unsafeShiftR w 6) .|. 0b10000000

byteFourFour :: Word -> Word
byteFourFour w = (0b00111111 .&. w) .|. 0b10000000

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

surrogate :: Word -> Bool
surrogate codepoint = codepoint >= 0xD800 && codepoint < 0xE000

