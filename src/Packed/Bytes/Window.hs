{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}

{-# OPTIONS_GHC
 -Weverything
 -fno-warn-unsafe
 -fno-warn-implicit-prelude
 -O2
#-}

module Packed.Bytes.Window
  ( findByte
  , foldl'
  , reverse
  , zipAnd
  , zipOr
  , zipXor
  , equality
    -- * Characters
  , isAscii
  , isUtf8
  , findNonAscii'
  ) where

import Data.Primitive (ByteArray(ByteArray))
import Data.Word (Word8)
import GHC.Types (RuntimeRep,TYPE)
import GHC.Int (Int(I#))
import GHC.Word (Word8(W8#),Word(W#))
import GHC.Exts (Int#,Word#,ByteArray#)
import Data.Bits (xor,(.|.),(.&.),complement,unsafeShiftL)
import Control.Monad.ST (ST,runST)
import qualified Data.Primitive as PM

type Maybe# (a :: TYPE (r :: RuntimeRep)) = (# (# #) | a #)

boxMaybeInt :: Maybe# Int# -> Maybe Int
boxMaybeInt = \case
  (# | a #) -> Just (I# a)
  (# (# #) | #) -> Nothing

unboxInt :: Int -> Int#
unboxInt (I# i) = i

unboxWord :: Word -> Word#
unboxWord (W# i) = i

-- | Finds the first occurrence of the given byte.
{-# INLINE findByte #-}
findByte :: Int -> Int -> Word8 -> ByteArray -> Maybe Int
findByte (I# off) (I# len) (W8# w) (ByteArray arr) =
  boxMaybeInt (findByte' off len w arr)

{-# NOINLINE findByte' #-}
findByte' :: Int# -> Int# -> Word# -> ByteArray# -> Maybe# Int#
findByte' !off# !len0# !w0# !arr0# = 
  let !off = I# off#
      !len0 = I# len0#
      !end0 = off + len0
      !beginMachWord = alignUp off
      !endMachWord = alignDown end0
   in if len0 < PM.sizeOf (undefined :: Word)
        then go off end0
        else case go off (beginMachWord * PM.sizeOf (undefined :: Word)) of
          (# | ix #) -> (# | ix #)
          (# (# #) | #) -> case goMachWord beginMachWord endMachWord (broadcastWord8 w) of
            (# | ix #) -> (# | ix #)
            (# (# #) | #) -> case go (endMachWord * PM.sizeOf (undefined :: Word)) end0 of
              (# | ix #) -> (# | ix #)
              (# (# #) | #) -> (# (# #) | #)
  where
  !w = W8# w0#
  !arr = ByteArray arr0#
  go :: Int -> Int -> Maybe# Int#
  go !ix !end = if ix < end
    then if PM.indexByteArray arr ix == w
      then (# | unboxInt ix #)
      else go (ix + 1) end
    else (# (# #) | #)
  -- The start and end index here are given in machine Word elements,
  -- not Word8 elements.
  goMachWord :: Int -> Int -> Word -> Maybe# Int#
  goMachWord !ix !end !artifact = if ix < end
    then case detectArtifact (unsafeIndexWord arr ix) artifact of
      0 -> goMachWord (ix + 1) end artifact
      _ -> go -- this call to go should always return Just
        (ix * PM.sizeOf (undefined :: Word)) 
        ((ix + 1) * PM.sizeOf (undefined :: Word))
    else (# (# #) | #)

-- cast a Word8 index to a machine Word index, rounding up
alignUp :: Int -> Int
alignUp i =
  let !(!quotient,!remainder) = quotRem i (PM.sizeOf (undefined :: Word))
   in case remainder of
        0 -> quotient
        _ -> quotient + 1

-- cast a Word8 index to a machine Word index, rounding down
alignDown :: Int -> Int
alignDown i = quot i (PM.sizeOf (undefined :: Word))

broadcastWord8 :: Word8 -> Word
broadcastWord8 !w0 = go 8 (fromIntegral w0) where
  go :: Int -> Word -> Word
  go !n !w = if n < 8 * PM.sizeOf (undefined :: Word)
    then go (twice n) (unsafeShiftL w n .|. w)
    else w

twice :: Int -> Int
twice n = n * 2

-- returns non-zero if a null byte is present in the machine word
detectNull :: Word -> Word
detectNull x = (x - repeatHexZeroOne) .&. complement x .&. repeatHexEightZero

detectArtifact :: Word -> Word -> Word
detectArtifact x artifact = detectNull (applyArtifact x artifact)

applyArtifact :: Word -> Word -> Word
applyArtifact = xor

repeatHexZeroOne :: Word
repeatHexZeroOne = div maxBound 255

repeatHexEightZero :: Word
repeatHexEightZero = 128 * (div maxBound 255 :: Word)

foldl' :: forall a. Int -> Int -> (a -> Word8 -> a) -> a -> ByteArray -> a
foldl' !off !len f !acc0 !arr = go acc0 off where
  go :: a -> Int -> a
  go !acc !ix = if ix < off + len
    then go (f acc (PM.indexByteArray arr ix)) (ix + 1)
    else acc

-- this is only used internally
unsafeIndexWord :: ByteArray -> Int -> Word
unsafeIndexWord = PM.indexByteArray

safeIndexWord :: ByteArray -> Int -> Word
safeIndexWord arr ix = if ix < 0 || ix >= (div (PM.sizeofByteArray arr) (PM.sizeOf (undefined :: Word)))
  then error ("safeIndexWord: " ++ show ix ++ " is out of bounds")
  else PM.indexByteArray arr ix

-- this is only used internally
unsafeIndex :: ByteArray -> Int -> Word8
unsafeIndex = PM.indexByteArray

safeIndex :: ByteArray -> Int -> Word8
safeIndex arr ix = if ix < 0 || ix >= PM.sizeofByteArray arr
  then error ("safeIndex: " ++ show ix ++ " is out of bounds")
  else PM.indexByteArray arr ix

-- TODO: optimize this. We could do a whole Word64 at a
-- time if the bytearray is pinned. Maybe even if it
-- isn't pinned.
-- reverse :: Int -> Int -> ByteArray -> ByteArray
-- reverse off len arr = runST

-- | Check if the given slice of the two byte arrays
--   is equal.
equality :: 
     Int -- ^ start x
  -> Int -- ^ start y
  -> Int -- ^ length
  -> ByteArray -- ^ array x
  -> ByteArray -- ^ array y
  -> Bool
equality !ixA !ixB !len !arrA !arrB = go 0
  -- TODO: Replace this with compareByteArrays# once GHC 8.4
  -- or GHC 8.6 is released. This will be simpler and much
  -- faster.
  where
  go :: Int -> Bool
  go !ix = if ix < len
    then if safeIndex arrA (ix + ixA) == safeIndex arrB (ix + ixB)
      then go (ix + 1)
      else False
    else True

{-# INLINE zipVectorizable #-}
zipVectorizable ::
     (Word8 -> Word8 -> Word8)
  -> (Word -> Word -> Word)
  -> Int -- start x
  -> Int -- len x
  -> Int -- start y
  -> Int -- len y
  -> ByteArray -- x
  -> ByteArray -- y
  -> ByteArray -- z
zipVectorizable !combine !combineMach !startX !lenX !startY !lenY !x !y = runST action
  where
  action :: forall s. ST s ByteArray
  action = do
    let !len = min lenX lenY
    marr <- PM.newByteArray len
    let !(!quotStartX,!remStartX) = quotRem startX (PM.sizeOf (undefined :: Word))
        !(!quotStartY,!remStartY) = quotRem startY (PM.sizeOf (undefined :: Word))
        go :: Int -> Int -> ST s ()
        go !ix !end = if ix < end
          then do
            PM.writeByteArray marr ix (combine (unsafeIndex x (startX + ix)) (unsafeIndex y (startY + ix)))
            go (ix + 1) end
          else return ()
        goMach :: Int -> Int -> ST s ()
        goMach !ix !end = if ix < end
          then do
            PM.writeByteArray marr ix (combineMach (unsafeIndexWord x (quotStartX + ix)) (unsafeIndexWord y (quotStartY + ix)))
            goMach (ix + 1) end
          else return ()
    if remStartX .|. remStartY == 0 -- if they are both zero
      then do
        let !lenQuotient = quot len (PM.sizeOf (undefined :: Word))
        goMach 0 lenQuotient
        go (lenQuotient * PM.sizeOf (undefined :: Word)) len
      else go 0 len
    PM.unsafeFreezeByteArray marr

zipAnd :: Int -> Int -> Int -> Int -> ByteArray -> ByteArray -> ByteArray
zipAnd x0 xlen y0 ylen x y = zipVectorizable (.&.) (.&.) x0 xlen y0 ylen x y

zipOr :: Int -> Int -> Int -> Int -> ByteArray -> ByteArray -> ByteArray
zipOr x0 xlen y0 ylen x y = zipVectorizable (.|.) (.|.) x0 xlen y0 ylen x y

zipXor :: Int -> Int -> Int -> Int -> ByteArray -> ByteArray -> ByteArray
zipXor x0 xlen y0 ylen x y = zipVectorizable (.|.) (.|.) x0 xlen y0 ylen x y

-- this can be used to implement all predicates or any predicates
{-# INLINE boolVectorizable #-}
boolVectorizable ::
     Bool
  -> (Bool -> Bool -> Bool)
  -> (Word8 -> Bool)
  -> (Word -> Bool)
  -> Int -- start
  -> Int -- len
  -> ByteArray
  -> Bool
boolVectorizable emptyBool together predicate predicateMach !start !len !arr =
  let !end0 = start + len
      !beginMachWord = alignUp start
      !endMachWord = alignDown end0
   in if len < PM.sizeOf (undefined :: Word)
        then go start end0
        else together
          (go start (beginMachWord * PM.sizeOf (undefined :: Word)))
          (together
            (goMachWord beginMachWord endMachWord)
            (go (endMachWord * PM.sizeOf (undefined :: Word)) end0)
          )
  where
  go :: Int -> Int -> Bool
  go !ix !end = if ix < end
    then together (predicate (PM.indexByteArray arr ix)) (go (ix + 1) end)
    else emptyBool
  -- The start and end index here are given in machine Word elements,
  -- not Word8 elements.
  goMachWord :: Int -> Int -> Bool
  goMachWord !ix !end = if ix < end
    then together (predicateMach (unsafeIndexWord arr ix)) (goMachWord (ix + 1) end)
    else emptyBool

{-# INLINE findVectorizable #-}
findVectorizable ::
     (Word8 -> Bool)
  -> (Word -> Bool)
  -> Int -- start
  -> Int -- len
  -> ByteArray
  -> Maybe# Int#
findVectorizable predicate predicateMach !start !len !arr =
  let !end0 = start + len
      !beginMachWord = alignUp start
      !endMachWord = alignDown end0
   in if len < PM.sizeOf (undefined :: Word)
        then go start end0
        else case go start (beginMachWord * PM.sizeOf (undefined :: Word)) of
          (# | ix #) -> (# | ix #)
          (# (# #) | #) -> case goMachWord beginMachWord endMachWord of
            (# | ix #) -> (# | ix #)
            (# (# #) | #) -> case go (endMachWord * PM.sizeOf (undefined :: Word)) end0 of
              (# | ix #) -> (# | ix #)
              (# (# #) | #) -> (# (# #) | #)
  where
  go :: Int -> Int -> Maybe# Int#
  go !ix !end = if ix < end
    then case predicate (safeIndex arr ix) of
      False -> go (ix + 1) end
      True -> (# | unboxInt ix #)
    else (# (# #) | #)
  -- The start and end index here are given in machine Word elements,
  -- not Word8 elements.
  goMachWord :: Int -> Int -> Maybe# Int#
  goMachWord !ix !end = if ix < end
    then case predicateMach (safeIndexWord arr ix) of
      False -> goMachWord (ix + 1) end
      True -> go
        (ix * PM.sizeOf (undefined :: Word)) 
        ((ix + 1) * PM.sizeOf (undefined :: Word))
    else (# (# #) | #)

asciiMask :: Word8
asciiMask = 0x80

asciiMachMask :: Word
asciiMachMask = repeatHexEightZero

isAscii :: Int -> Int -> ByteArray -> Bool
isAscii start len arr = boolVectorizable True (&&)
  (\w -> w .&. asciiMask == 0)
  (\w -> w .&. asciiMachMask == 0)
  start len arr

-- This could be exported but it does not seem generally useful.
findNonAscii' :: Int -> Int -> ByteArray -> Maybe# Int#
findNonAscii' !start !len !arr = findVectorizable
  (\w -> w .&. asciiMask /= 0)
  (\w -> w .&. asciiMachMask /= 0)
  start len arr

-- | The meaning of the result sum elements in order:
--
--   1. An integer representing the first unparseable byte. This
--      may be up to three bytes before the actual byte where
--      the actual problem occurred. The leading byte is given
--      since that gives recovery mechanisms a reasonable place
--      to split the Bytes.
--   2. An empty nullary tuple indicating success with no leftovers
--   3. A triple of three words. The first word is the total number
--      of bytes in the multibyte word (only ever 2, 3, or 4). The
--      second is the number of additional bytes needed (only
--      ever 1, 2, or 3). The third is the fragment of the character built
--      so far. It will need to be bit shifted to the left by some
--      multiple of 6 to be completed.
--   
--   The first tuple element is 0 if everything successfully
--   parsed was acsii and a word with the high bit set to 1
--   if multi-byte characters were present.
--   It is 1 if there were surrogates, characters
--   in the range @U+D800@ to @U+DFFF@, present in the text. If
--   there are surrogates, it is implied that there are multi-byte
--   characters, since a surrogate is multi-byte by definition.
--   The value of this element is still meaningful even if the parse
--   ultimately fails.
isUtf8 :: 
     Int -- start
  -> Int -- length
  -> ByteArray -- bytes
  -> (# Word#, (# Int# | (# #) | (# Word#, Word#, Word# #) #) #)
isUtf8 !start !len !arr = case findNonAscii' start len arr of
  (# (# #) | #) -> (# 0##, (# | (# #) | #) #)
  (# | ix# #) -> case postAsciiIsUtf8 (I# ix#) (len + start - (I# ix#)) arr of
    (# hasSurrogate, (# ixFailure# | | #) #) -> if I# ix# == I# ixFailure#
      then (# 0## , (# ixFailure# | | #) #)
      else (# hasSurrogate, (# ixFailure# | | #) #)
    (# hasSurrogate, (# | (# #) | #) #) -> (# hasSurrogate, (# | (# #) | #) #)
    (# hasSurrogate, (# | | (# w1, w2, w3 #) #) #) -> (# hasSurrogate, (# | | (# w1, w2, w3 #) #) #)
-- Notes on the implementation of isUtf8 There is some careful trickery to
-- ensure that we always correctly report whether or not we encountered any
-- multi-byte characters. We initially do a fast run to get as far as we can
-- on only ascii characters. In this stage, we are able to travel a full
-- machine word at a time. After this, we switch to a slower byte-by-byte
-- UTF-8 recognition function.  If this second stage fails on the very first
-- byte it sees, we report that everything successfully parsed was ascii.
-- If it fails at any point after this, we report that multibyte characters
-- were encounter. If it succeeds, we report that we encounter multi-byte
-- characters. This is accurate because, for it to start running at all,
-- findNonAscii' must have found something that was not ascii.

-- Note that postAsciiIsUtf8 does not return anything indicating whether or not there
-- were any multi-byte characters present. This is because, if this function
-- is called, it should be called with the start index on a non-ascii-encoded
-- character. This function is not expect to perform well. It shouldn't
-- allocate memory, but it has to go byte-by-byte through the ByteArray. The
-- author of this library does not know of a way to vectorize the check
-- for UTF-8 compliance.
--
-- The first element of the response tuple is either 1 or a machine word
-- with the high bit set to 1. If it is the high bit,
-- no surrogates were present. If it is 1, surrogates were present.
-- The second element is a nested unboxed sum with three cases. These
-- are described in the docs for isUtf8.
--
postAsciiIsUtf8 :: 
     Int -- start
  -> Int -- length
  -> ByteArray -- bytes
  -> (# Word#, (# Int# | (# #) | (# Word#, Word#, Word# #) #) #)
postAsciiIsUtf8 !start !len !arr = go start binaryOneThenZeroes
  where
  !end = start + len
  go :: Int 
     -> Word
     -> (# Word#, (# Int# | (# #) | (# Word#, Word#, Word# #) #) #)
  go !ix !hasSurrogate = if ix < end
    then
      let !firstByte = safeIndex arr ix in
       if | oneByteChar firstByte -> go (ix + 1) hasSurrogate
          | twoByteChar firstByte -> if ix + 1 < end
              then if followingByte (safeIndex arr (ix + 1))
                then go (ix + 2) hasSurrogate
                else (# unboxWord hasSurrogate, (# unboxInt ix | | #) #)
              else (# unboxWord hasSurrogate, (# | | (# 2##, 1##, unboxWord (byteTwoPartialOne firstByte) #) #) #)
          | threeByteChar firstByte ->
              if | ix + 2 < end -> 
                     let !secondByte = safeIndex arr (ix + 1) in
                     if followingByte secondByte
                       then 
                         let !thirdByte = safeIndex arr (ix + 2) in
                         if followingByte thirdByte
                           then if surrogate (codepointFromThreeBytes firstByte secondByte thirdByte)
                             then go (ix + 3) 1
                             else go (ix + 3) hasSurrogate
                           else (# unboxWord hasSurrogate, (# unboxInt ix | | #) #)
                       else (# unboxWord hasSurrogate, (# unboxInt ix | | #) #)
                 | ix + 1 < end -> 
                     let !secondByte = safeIndex arr (ix + 1) in
                     if followingByte secondByte
                       then (# unboxWord hasSurrogate, (# | | (# 3##, 1##, unboxWord (byteThreePartialTwo firstByte secondByte) #) #) #)
                       else (# unboxWord hasSurrogate, (# unboxInt ix | | #) #)
                 | otherwise -> (# unboxWord hasSurrogate, (# | | (# 3##, 2##, unboxWord (byteThreePartialOne firstByte) #) #) #)
          | fourByteChar firstByte ->
              if | ix + 3 < end ->
                     let !secondByte = safeIndex arr (ix + 1) in
                     if followingByte secondByte
                       then 
                         let !thirdByte = safeIndex arr (ix + 2) in
                         if followingByte thirdByte
                           then
                             let !fourthByte = safeIndex arr (ix + 3) in
                             if followingByte fourthByte
                               then go (ix + 4) hasSurrogate
                               else (# unboxWord hasSurrogate, (# unboxInt ix | | #) #)
                           else (# unboxWord hasSurrogate, (# unboxInt ix | | #) #)
                       else (# unboxWord hasSurrogate, (# unboxInt ix | | #) #)
                 | ix + 2 < end -> 
                     let !secondByte = safeIndex arr (ix + 1) in
                     if followingByte secondByte
                       then 
                         let !thirdByte = safeIndex arr (ix + 2) in
                         if followingByte thirdByte
                           then (# unboxWord hasSurrogate, (# | | (# 4##, 1##, unboxWord (byteFourPartialThree firstByte secondByte thirdByte) #) #) #)
                           else (# unboxWord hasSurrogate, (# unboxInt ix | | #) #)
                       else (# unboxWord hasSurrogate, (# unboxInt ix | | #) #)
                 | ix + 1 < end -> 
                     let !secondByte = safeIndex arr (ix + 1) in
                     if followingByte secondByte
                       then (# unboxWord hasSurrogate, (# | | (# 4##, 2##, unboxWord (byteFourPartialTwo firstByte secondByte) #) #) #)
                       else (# unboxWord hasSurrogate, (# unboxInt ix | | #) #)
                 | otherwise -> (# unboxWord hasSurrogate, (# | | (# 4##, 3##, unboxWord (byteFourPartialOne firstByte) #) #) #)
          | otherwise -> (# unboxWord hasSurrogate, (# unboxInt ix | | #) #)
    else (# unboxWord hasSurrogate, (# | (# #) | #) #)

byteTwoPartialOne :: Word8 -> Word
byteTwoPartialOne w = word8ToWord w .&. 0b00011111

byteThreePartialTwo :: Word8 -> Word8 -> Word
byteThreePartialTwo a b =
  unsafeShiftL (word8ToWord a .&. 0b00001111) 6 .|.
  (word8ToWord b .&. 0b00111111)

byteThreePartialOne :: Word8 -> Word
byteThreePartialOne a = word8ToWord a .&. 0b00001111

byteFourPartialTwo :: Word8 -> Word8 -> Word
byteFourPartialTwo a b =
  unsafeShiftL (word8ToWord a .&. 0b00000111) 6 .|.
  (word8ToWord b .&. 0b00111111)

byteFourPartialThree :: Word8 -> Word8 -> Word8 -> Word
byteFourPartialThree a b c =
  unsafeShiftL (word8ToWord a .&. 0b00000111) 12 .|.
  unsafeShiftL (word8ToWord b .&. 0b00111111) 6 .|.
  (word8ToWord c .&. 0b00111111)

byteFourPartialOne :: Word8 -> Word
byteFourPartialOne a = word8ToWord a .&. 0b00000111

followingByte :: Word8 -> Bool
followingByte !w = xor w 0b01000000 .&. 0b11000000 == 0b11000000

oneByteChar :: Word8 -> Bool
oneByteChar !w = w .&. 0b10000000 == 0

twoByteChar :: Word8 -> Bool
twoByteChar !w = w .&. 0b11100000 == 0b11000000

threeByteChar :: Word8 -> Bool
threeByteChar !w = w .&. 0b11110000 == 0b11100000

fourByteChar :: Word8 -> Bool
fourByteChar !w = w .&. 0b11111000 == 0b11110000

codepointFromThreeBytes :: Word8 -> Word8 -> Word8 -> Word
codepointFromThreeBytes w1 w2 w3 = 
  unsafeShiftL (word8ToWord w1 .&. 0b00001111) 12 .|. 
  unsafeShiftL (word8ToWord w2 .&. 0b00111111) 6 .|. 
  (word8ToWord w3 .&. 0b00111111)

word8ToWord :: Word8 -> Word
word8ToWord = fromIntegral

surrogate :: Word -> Bool
surrogate codepoint = codepoint >= 0xD800 && codepoint < 0xE000

binaryOneThenZeroes :: Word
binaryOneThenZeroes = maxBound - div (maxBound :: Word) 2

