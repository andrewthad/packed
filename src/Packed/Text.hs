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

module Packed.Text
  ( Text(..)
  , empty
  , singleton
  , pack
  , unpack
  , breakChar
  , map
  , toUpper
  , take
  , drop
  , dropEnd
  , length
    -- * Encoding
  , decodeAscii
  , decodeResumeAscii
  , decodeUtf8
  , decodeResumeUtf8
  , encodeUtf8
  ) where

import Prelude hiding (map,take,drop,length)
import Data.Char (ord,chr)
import Data.Primitive (MutableByteArray)
import Packed.Bytes.Small (ByteArray)
import GHC.Exts (Word#)
import GHC.Int (Int(I#))
import GHC.Word (Word(W#),Word8(W8#))
import Data.Bits ((.&.),(.|.),unsafeShiftR,unsafeShiftL,complement)
import Control.Monad.ST (ST,runST)
import Packed.Bytes (Bytes(..))
import Data.String (IsString(fromString))
import qualified Packed.Text.Window as TW
import qualified Data.Char
import qualified Packed.Bytes.Small as BA
import qualified Packed.Bytes.Window as BAW
import qualified Packed.Bytes as B
import qualified Data.Primitive as PM

data Text = Text
  {-# UNPACK #-} !ByteArray -- payload, normal UTF8-encoded text, nothing special like the unsliced variant
  {-# UNPACK #-} !Word -- offset in bytes, not in characters, first bit reserved
  {-# UNPACK #-} !Int -- length in bytes, not in characters
-- The byte-multiplicity for text is always Multiple if
-- a multi-byte character is present. If no multi-byte
-- characters are present, it may be set to either single
-- or multiple. Functions should try to set it to single
-- when possible.

instance IsString Text where
  fromString = pack

instance Semigroup Text where
  t1 <> t2 = Text arr (buildOffMult 0 (appendMult mult1 mult2)) (len1 + len2)
    where
    !(!arr1,!off1,!len1,!mult1) = textMatch t1
    !(!arr2,!off2,!len2,!mult2) = textMatch t2
    !arr = runST $ do
      marr <- PM.newByteArray (len1 + len2)
      PM.copyByteArray marr 0 arr1 off1 len1
      PM.copyByteArray marr len1 arr2 off2 len2
      PM.unsafeFreezeByteArray marr

instance Monoid Text where
  mempty = empty

instance Eq Text where
  t1 == t2 = Bytes arr1 off1 len1 == Bytes arr2 off2 len2
    where
    !(!arr1,!off1,!len1,!_) = textMatch t1
    !(!arr2,!off2,!len2,!_) = textMatch t2

instance Show Text where
  show = show . unpack 

newtype Multiplicity = Multiplicity Word
  deriving Eq

appendMult :: Multiplicity -> Multiplicity -> Multiplicity
appendMult (Multiplicity a) (Multiplicity b) = Multiplicity (a .|. b)

single :: Multiplicity
single = Multiplicity 0

multiple :: Multiplicity
multiple = Multiplicity binaryOneThenZeroes

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

singleton :: Char -> Text
singleton c = let x = TW.singleton c in Text x 0 (PM.sizeofByteArray x)

unpack :: Text -> String
unpack !t = TW.unpack off len arr
  where
  !(!arr,!off,!len,!_) = textMatch t

pack :: String -> Text
pack s = case TW.pack s of
  (arr,mult) -> Text arr (buildOffMult 0 (Multiplicity mult)) (BA.length arr)

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

nextCharIx :: ByteArray -> Int -> Int
nextCharIx !arr !ix
  | oneByteChar firstByte = ix + 1
  | twoByteChar firstByte = ix + 2
  | threeByteChar firstByte = ix + 3
  | otherwise = ix + 4
  where
  firstByte :: Word8
  !firstByte = BA.unsafeIndex arr ix

moveChars ::
     ByteArray -- array
  -> Int -- start index
  -> Int -- maximal index
  -> Int -- number of characters to move through
  -> Int -- end index
moveChars !arr !start0 !maxIndex !n0 = go start0 n0
  where
  go :: Int -> Int -> Int
  go !ix !n = if n > 0 && ix < maxIndex
    then go (nextCharIx arr ix) (n - 1)
    else ix

countChars ::
     ByteArray -- array
  -> Int -- start index
  -> Int -- maximal index
  -> Int -- number of characters 
countChars !arr !start0 !maxIndex = go start0 0
  where
  go :: Int -> Int -> Int
  go !ix !acc = if ix < maxIndex
    then go (nextCharIx arr ix) (acc + 1)
    else acc

oneByteChar :: Word8 -> Bool
oneByteChar w = w .&. 0b10000000 == 0

twoByteChar :: Word8 -> Bool
twoByteChar w = w .&. 0b11100000 == 0b11000000

threeByteChar :: Word8 -> Bool
threeByteChar w = w .&. 0b11110000 == 0b11100000

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

breakChar :: Char -> Text -> (Text,Text)
breakChar !c !t
  | codepoint < 0x80 = breakOnByte1 (unsafeWordToWord8 codepoint) t
  | codepoint < 0x800 = case findBytePair off len (unsafeWordToWord8 (byteTwoOne codepoint)) (unsafeWordToWord8 (byteTwoTwo codepoint)) arr of
      Nothing -> (t,empty)
      Just ix -> (Text arr (buildOffMult off mult) (ix - off), Text arr (buildOffMult ix mult) (len + off - ix))
  | surrogate codepoint = case findByteTriple off len 0xEF 0xBF 0xBD arr of
      Nothing -> (t,empty)
      Just ix -> (Text arr (buildOffMult off mult) (ix - off), Text arr (buildOffMult ix mult) (len + off - ix))
  | codepoint < 0x10000 = case findByteTriple off len (unsafeWordToWord8 (byteThreeOne codepoint)) (unsafeWordToWord8 (byteThreeTwo codepoint)) (unsafeWordToWord8 (byteThreeThree codepoint)) arr of
      Nothing -> (t,empty)
      Just ix -> (Text arr (buildOffMult off mult) (ix - off), Text arr (buildOffMult ix mult) (len + off - ix))
  | otherwise = case findByteQuadruple off len (unsafeWordToWord8 (byteFourOne codepoint)) (unsafeWordToWord8 (byteFourTwo codepoint)) (unsafeWordToWord8 (byteFourThree codepoint)) (unsafeWordToWord8 (byteFourFour codepoint)) arr of
      Nothing -> (t,empty)
      Just ix -> (Text arr (buildOffMult off mult) (ix - off), Text arr (buildOffMult ix mult) (len + off - ix))
  where
  !codepoint = intToWord (ord c)
  !(!arr,!off,!len,!mult) = textMatch t

findBytePair :: Int -> Int -> Word8 -> Word8 -> ByteArray -> Maybe Int
findBytePair off0 len0 w1 w2 arr = go off0 (off0 + len0 - 1)
  where
  go :: Int -> Int -> Maybe Int
  go !ix !end = if ix < end
    then if PM.indexByteArray arr ix == w1 && PM.indexByteArray arr (ix + 1) == w2
      then Just ix
      else go (ix + 1) end
    else Nothing

findByteTriple :: Int -> Int -> Word8 -> Word8 -> Word8 -> ByteArray -> Maybe Int
findByteTriple off0 len0 w1 w2 w3 arr = go off0 (off0 + len0 - 2)
  where
  go :: Int -> Int -> Maybe Int
  go !ix !end = if ix < end
    then if PM.indexByteArray arr ix == w1 && PM.indexByteArray arr (ix + 1) == w2 && PM.indexByteArray arr (ix + 2) == w3
      then Just ix
      else go (ix + 1) end
    else Nothing

findByteQuadruple :: Int -> Int -> Word8 -> Word8 -> Word8 -> Word8 -> ByteArray -> Maybe Int
findByteQuadruple off0 len0 w1 w2 w3 w4 arr = go off0 (off0 + len0 - 3)
  where
  go :: Int -> Int -> Maybe Int
  go !ix !end = if ix < end
    then if PM.indexByteArray arr ix == w1 && PM.indexByteArray arr (ix + 1) == w2 && PM.indexByteArray arr (ix + 2) == w3 && PM.indexByteArray arr (ix + 3) == w4
      then Just ix
      else go (ix + 1) end
    else Nothing

-- precondition: the Word is less than 0x80
breakOnByte1 :: Word8 -> Text -> (Text,Text)
breakOnByte1 !w !t =
  let !(!arr,!off,!len,!mult) = textMatch t
   in case BAW.findByte off len w arr of
        Nothing -> (t,empty)
        Just !ix -> (dwindle (Text arr (buildOffMult off mult) (ix - off)), Text arr (buildOffMult ix mult) (len + off - ix))

dwindle :: Text -> Text
dwindle t@(Text _ _ !len) = if len > 0 then t else empty
  
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

-- Each byte in the word that is a lowercase ascii character is turned
-- into 0x80. All other bytes become 0x00.
hasAsciiLowerArtifact :: Word -> Word
hasAsciiLowerArtifact w = 
  ((div maxBound 255 * (127 + hi)) - (w .&. (div maxBound 255 * 127))) .&. complement w .&.
  ((w .&. (div maxBound 255 * 127)) + (div maxBound 255 * (127 - lo))) .&. (div maxBound 255 * 128)
  where
  lo = intToWord (ord 'a' - 1)
  hi = intToWord (ord 'z' + 1)

-- TODO: improve this. Currently, we do not use a vectorized implementation if
-- the string doesn't start aligned on a machine word address. We can fix this
-- by padding the result to the left to line it up better. This would trade a
-- small amount of space for more speed.
{-# INLINE mapVectorizable #-}
mapVectorizable ::
     (Word8 -> Word8) -- function
  -> (Word -> Word) -- vectorized function variant
  -> Int -- start
  -> Int -- len
  -> ByteArray -- array
  -> ByteArray
mapVectorizable !func !funcMach !start !len !arr = runST action
  where
  action :: forall s. ST s ByteArray
  action = do
    marr <- PM.newByteArray len
    let !(!quotStart,!remStart) = quotRem start (PM.sizeOf (undefined :: Word))
        go :: Int -> Int -> ST s ()
        go !ix !end = if ix < end
          then do
            PM.writeByteArray marr ix (func (PM.indexByteArray arr (start + ix)))
            go (ix + 1) end
          else return ()
        goMach :: Int -> Int -> ST s ()
        goMach !ix !end = if ix < end
          then do
            PM.writeByteArray marr ix (funcMach (PM.indexByteArray arr (quotStart + ix)))
            goMach (ix + 1) end
          else return ()
    if remStart == 0
      then do
        let !lenQuotient = quot len (PM.sizeOf (undefined :: Word))
        goMach 0 lenQuotient
        go (lenQuotient * PM.sizeOf (undefined :: Word)) len
      else go 0 len
    PM.unsafeFreezeByteArray marr

toUpperAsciiWord8 :: Word8 -> Word8
toUpperAsciiWord8 w = if word8ToWord w - intToWord (ord 'a') < 26
  then w - 0x20
  else w

toUpperAsciiWord :: Word -> Word
toUpperAsciiWord w = w - unsafeShiftR (hasAsciiLowerArtifact w) 2

toUpperAscii :: Int -> Int -> ByteArray -> ByteArray
toUpperAscii !off !len !arr = mapVectorizable toUpperAsciiWord8 toUpperAsciiWord off len arr

toUpper :: Text -> Text
toUpper t = if mult == single
  then Text (toUpperAscii off len arr) (buildZeroOffMult single) len
  else map Data.Char.toUpper t
  where
  !(!arr,!off,!len,!mult) = textMatch t

map :: (Char -> Char) -> Text -> Text
map f !t = runST action
  where
  !(!arr,!off,!len,!_) = textMatch t
  action :: ST s Text
  action = do
    marr0 <- PM.newByteArray (len + 3)
    let go :: Int -> Int -> Int -> Multiplicity -> MutableByteArray s -> ST s (Int,MutableByteArray s,Multiplicity)
        go !ixSrc !ixDst !marrLen !mult !marr = if ixSrc < off + len
          then do
            let !(!ixSrcNext,!c) = nextChar arr ixSrc
                !c' = f c
                -- It is disappointing that this is handled this
                -- way. Reconsider this later.
                !newMult = appendMult mult (if ixSrcNext - ixSrc > 1 then multiple else single)
            if ixDst < marrLen - 3
              then do
                ixDstNext <- writeChar c' ixDst marr
                go ixSrcNext ixDstNext marrLen newMult marr
              else do
                let newMarrLen = marrLen * 2
                newMarr <- PM.newByteArray newMarrLen
                PM.copyMutableByteArray newMarr 0 marr 0 marrLen -- possible minus 3?
                ixDstNext <- writeChar c' ixDst newMarr
                go ixSrcNext ixDstNext newMarrLen newMult newMarr
          else return (ixDst,marr,mult)
    (finalLen,finalMarr,finalMult) <- go off 0 (len + 3) single marr0
    newArr <- PM.unsafeFreezeByteArray finalMarr
    return (Text newArr (buildZeroOffMult finalMult) finalLen)

-- | /O(n)/ 'take' @n xs@ returns the prefix of @xs@ of length @n@. It returns
--   @xs@ instead when @n > 'length' xs@. On text containing only ASCII characters,
--   the complexity of this function is reduced to /O(1)/.
take :: Int -> Text -> Text
take !n !t@(Text _ !offMult _) = if n < 1
  then empty
  else if mult == single
    then if n < len
      then Text arr offMult n
      else t
    else if n < len * 4
      then Text arr offMult (moveChars arr off (off + len) n - off)
      else t
  where
  !(!arr,!off,!len,!mult) = textMatch t

-- | /O(n)/ 'drop' @n xs@ returns the suffix of @xs@ after the first @n@ characters
--   have been removed. It returns @empty@ instead when @n > 'length' xs@. On text
--   containing only ASCII characters, the complexity of this function is reduced to /O(1)/.
drop :: Int -> Text -> Text
drop !n !t = if n < 1
  then t
  else if mult == single
    then if n < len
      then Text arr (buildOffMult (off + n) mult) (len - n)
      else empty
    else if n < len * 4
      then
        let !skipped = moveChars arr off (off + len) n - off
         in Text arr (buildOffMult (off + skipped) mult) (len - skipped)
      else empty
  where
  !(!arr,!off,!len,!mult) = textMatch t

length :: Text -> Int
length !t = if mult == single
  then len
  else countChars arr off (off + len)
  where
  !(!arr,!off,!len,!mult) = textMatch t

-- | /O(n)/ 'dropEnd' @n xs@ returns the prefix of @xs@ after the last @n@ characters
--   have been removed. It returns @empty@ instead when @n > 'length' xs@. On text
--   containing only ASCII characters, the complexity of this function is reduced to /O(1)/.
dropEnd :: Int -> Text -> Text
-- Note: There is a way to implement this that is more efficient. It would
-- required scanning UTF-8 encoded text backwards, which seems annoying
-- to do.
dropEnd !n !t = take (length t - n) t

decodeAscii :: Bytes -> Maybe Text
decodeAscii b = case decodeResumeAscii b of
  (# t, (# | (# #) #) #) -> Just t
  (# !_, (# !_ | #) #) -> Nothing
-- decodeAscii b@(Bytes arr off len) = if B.isAscii b
--   then Just (Text arr (buildOffMult off single) len)
--   else Nothing

decodeResumeAscii :: Bytes -> (# Text, (# Bytes | (# #) #) #)
decodeResumeAscii (Bytes arr off len) = case BAW.findNonAscii' off len arr of
  (# (# #) | #) -> (# Text arr (buildOffMult off single) len, (# | (# #) #) #)
  (# | ix# #) -> 
    let ix = I# ix#
     in (# Text arr (buildOffMult off single) ix, (# (Bytes arr ix (len + off - ix)) | #) #)

decodeUtf8 :: Bytes -> Maybe Text
decodeUtf8 b = case decodeResumeUtf8 b of
  (# !_, (# !_ | | #) #) -> Nothing
  (# !t, (# | (# #) | #) #) -> Just t
  (# !_, (# | | !_ #) #) -> Nothing

decodeResumeUtf8 ::
     Bytes
  -> (# Text, (# Bytes | (# #) | (# Word#, Word#, Word# #) #) #)
decodeResumeUtf8 (Bytes arr off len) = 
  let !(# !ascii, !r #) = BAW.isUtf8 off len arr
      !modifiedArr = case ascii of
        1## -> runST $ do
          marr <- PM.newByteArray len
          PM.copyByteArray marr 0 arr off len
          replaceSurrogates 0 len marr
          PM.unsafeFreezeByteArray marr
        _ -> arr
      !mult = case ascii of
        1## -> multiple
        _ -> Multiplicity (W# ascii)
   in case r of
        (# ixFailure# | | #) -> 
          let !ixFailure = I# ixFailure#
           in (# Text modifiedArr (buildOffMult off mult) ixFailure, (# (Bytes arr ixFailure (len + off - ixFailure)) | | #) #)
        (# | (# #) | #) -> (# Text modifiedArr (buildOffMult off mult) len, (# | (# #) | #) #)
        (# | | (# w1, w2, w3 #) #) -> (# Text modifiedArr (buildOffMult off mult) len , (# | | (# w1, w2, w3 #) #) #)

replaceSurrogates :: forall s. Int -> Int -> MutableByteArray s -> ST s ()
replaceSurrogates start len marr = go start where
  go :: Int -> ST s ()
  go !ix = if ix < len - 2
    then do
      !w1 <- PM.readByteArray marr ix
      !w2 <- PM.readByteArray marr (ix + 1)
      !w3 <- PM.readByteArray marr (ix + 2)
      if threeByteChar w1 && surrogate (codepointFromThreeBytes w1 w2 w3)
        then do
          -- Codepoint U+FFFD
          PM.writeByteArray marr ix (0xEF :: Word8)
          PM.writeByteArray marr (ix + 1) (0xBF :: Word8)
          PM.writeByteArray marr (ix + 2) (0xBD :: Word8)
          go (ix + 3)
        else go (ix + 1)
    else return ()

codepointFromThreeBytes :: Word8 -> Word8 -> Word8 -> Word
codepointFromThreeBytes w1 w2 w3 = 
  unsafeShiftL (word8ToWord w1 .&. 0b00001111) 12 .|. 
  unsafeShiftL (word8ToWord w2 .&. 0b00111111) 6 .|. 
  (word8ToWord w3 .&. 0b00111111)

encodeUtf8 :: Text -> Bytes
encodeUtf8 t = Bytes arr off len where
  !(!arr,!off,!len,!_) = textMatch t

