{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC
 -Weverything
 -fno-warn-unsafe
 -fno-warn-implicit-prelude
 -fno-warn-missing-import-lists
 -O2
#-}

module Text.Slice
  ( Text(..)
  , empty
  , pack
  , unpack
  , breakOnChar
  ) where

import Data.Char (ord,chr)
import Byte.Array (ByteArray)
import GHC.Word (Word(W#),Word8(W8#))
import Data.Bits ((.&.),(.|.),unsafeShiftR,unsafeShiftL)
import Control.Monad.ST (ST,runST)
import qualified Byte.Array as BA
import qualified Byte.Array.Window as BAW
import qualified Data.Primitive as PM

data Text = Text
  {-# UNPACK #-} !ByteArray -- payload, normal UTF8-encoded text, nothing special like the unsliced variant
  {-# UNPACK #-} !Word -- offset in bytes, not in characters, first bit reserved
  {-# UNPACK #-} !Int -- length in bytes, not in characters

newtype Multiplicity = Multiplicity Word

appendMult :: Multiplicity -> Multiplicity -> Multiplicity
appendMult (Multiplicity a) (Multiplicity b) = Multiplicity (a .|. b)

single :: Multiplicity
single = Multiplicity 0

multiple :: Multiplicity
multiple = Multiplicity binaryOneThenZeroes

pack :: String -> Text
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
     in Text arr (buildZeroOffMult totalMult) totalBytes
  where
  metadata :: Int -> Multiplicity -> [Char] -> (Int,Multiplicity)
  metadata !totalBytes !totalMult [] = (totalBytes,totalMult)
  metadata !totalBytes !totalMult (!c : cs) =
    let !bytes = charBytes c
        !mult = if bytes < 2 then single else multiple
     in metadata (bytes + totalBytes) (appendMult mult totalMult) cs

-- result is between 1 and 4
charBytes :: Char -> Int
charBytes !c
  | codepoint < 0x80 = 1
  | codepoint < 0x800 = 2
  | codepoint < 0x10000 = 3
  | otherwise = 4
  where
  !codepoint = intToWord (ord c)

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

unpack :: Text -> String
unpack !t = go off
  where
  go :: Int -> String
  go !ix0 = if ix0 < len + off
    then
      let !(!ix1,!c) = nextChar arr ix0
       in c : go ix1
    else []
  !(!arr,!off,!len,!_) = textMatch t

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

oneByteChar :: Word8 -> Bool
oneByteChar w = w .&. 0b10000000 == 0

-- this only works if you already know that it is not a
-- single-byte character.
twoByteChar :: Word8 -> Bool
twoByteChar w = w .&. 0b00100000 == 0

threeByteChar :: Word8 -> Bool
threeByteChar w = w .&. 0b00010000 == 0

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

-- precondition: codepoint is less than 0x800
byteTwoOne :: Word -> Word
byteTwoOne w = unsafeShiftR w 6 .|. 0b1100000

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



word8ToWord :: Word8 -> Word
word8ToWord = fromIntegral

intToWord :: Int -> Word
intToWord = fromIntegral

wordToInt :: Word -> Int
wordToInt = fromIntegral

unsafeWordToWord8 :: Word -> Word8
unsafeWordToWord8 (W# w) = W8# w

wordToChar :: Word -> Char
wordToChar w = chr (fromIntegral w)

empty :: Text
empty = Text BA.empty 0 0

breakOnChar :: Char -> Text -> (Text,Text)
breakOnChar !c !t
  | codepoint < 0x80 = breakOnByte1 (unsafeWordToWord8 codepoint) t
  | codepoint < 0x800 = error "breakOnChar: two-byte chars"
  | surrogate codepoint = error "breakOnChar: surrogate chars"
  | codepoint < 0x10000 = error "breakOnChar: three-byte chars"
  | otherwise = error "breakOnChar: four-byte chars"
  where
  !codepoint = intToWord (ord c)

-- precondition: the Word is less than 0x80
breakOnByte1 :: Word8 -> Text -> (Text,Text)
breakOnByte1 !w !t =
  let !(!arr,!off,!len,!mult) = textMatch t
   in case BAW.findByte off len w arr of
        Nothing -> (t,empty)
        Just !ix -> (Text arr (buildOffMult off mult) ix, Text arr (buildOffMult ix mult) (len + off - ix))
  
buildOffMult :: Int -> Multiplicity -> Word
buildOffMult i (Multiplicity x) = intToWord i .|. x

buildZeroOffMult :: Multiplicity -> Word
buildZeroOffMult (Multiplicity w) = w

textMatch :: Text -> (ByteArray,Int,Int,Multiplicity)
textMatch (Text arr offMult len) =
  ( arr
  , wordToInt (binaryZeroThenOnes .&. offMult)
  , len
  , Multiplicity (offMult .&. binaryOneThenZeroes)
  )

binaryOneThenZeroes :: Word
binaryOneThenZeroes = maxBound - div (maxBound :: Word) 2

binaryZeroThenOnes :: Word
binaryZeroThenOnes = div (maxBound :: Word) 2


surrogate :: Word -> Bool
surrogate codepoint = codepoint >= 0xD800 && codepoint < 0xE000
