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
  , findMemberByte
  , findNonMemberByte
  , stripPrefixResumable
    -- * Hashing
  , hash
  , hashWith
  , slice
  , equals
    -- * Characters
  , isAscii
  , isUtf8
  , findNonAscii'
  , findNonAsciiSpace'
  ) where

import Data.Primitive (ByteArray(ByteArray))
import Data.Word (Word8)
import GHC.Types (RuntimeRep,TYPE)
import GHC.Int (Int(I#))
import GHC.Word (Word8(W8#),Word(W#))
import GHC.Exts (Int#,Word#,ByteArray#,byteSwap#,word2Int#,(+#),int2Word#)
import Data.Bits (xor,(.|.),(.&.),complement,unsafeShiftL,finiteBitSize,
  unsafeShiftR,countLeadingZeros)
import Control.Monad.ST (ST,runST)
import Packed.Bytes.Set (ByteSet)
import qualified Data.Primitive as PM
import qualified Packed.Bytes.Set as ByteSet

type Maybe# (a :: TYPE (r :: RuntimeRep)) = (# (# #) | a #)

boxMaybeInt :: Maybe# Int# -> Maybe Int
boxMaybeInt = \case
  (# | a #) -> Just (I# a)
  (# (# #) | #) -> Nothing

unboxInt :: Int -> Int#
unboxInt (I# i) = i

unboxWord :: Word -> Word#
unboxWord (W# i) = i

slice ::
     Int
  -> Int 
  -> ByteArray
  -> ByteArray
slice off len arr =
  if off == 0 && PM.sizeofByteArray arr == len
    then arr
    else runST $ do
      marr <- PM.newByteArray len
      PM.copyByteArray marr 0 arr off len
      PM.unsafeFreezeByteArray marr

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

-- | Search for the 'ByteArray' for any member byte. Returns 'Nothing' if nothing
--   from the 'ByteSet' is found. Returns 'Just' with both the index of the byte
--   and the byte itself if a matching byte is found.
findMemberByte :: Int -> Int -> ByteSet -> ByteArray -> Maybe (Int,Word8)
findMemberByte !(I# off) !(I# len) !(ByteSet.ByteSet (ByteArray set)) !(ByteArray arr) =
  case findMemberByte' off len set arr of
    (# (# #) | #) -> Nothing
    (# | (# i, w #) #) -> Just (I# i, W8# w)

findNonMemberByte :: Int -> Int -> ByteSet -> ByteArray -> Maybe (Int,Word8)
findNonMemberByte !(I# off) !(I# len) !(ByteSet.ByteSet (ByteArray set)) !(ByteArray arr) =
  case findNonMemberByte' off len set arr of
    (# (# #) | #) -> Nothing
    (# | (# i, w #) #) -> Just (I# i, W8# w)

{-# NOINLINE findMemberByte' #-}
findMemberByte' :: Int# -> Int# -> ByteArray# -> ByteArray# -> Maybe# (# Int# , Word# #)
findMemberByte' off# len# set# arr# = findTemplateMemberByte' id off# len# set# arr#

{-# NOINLINE findNonMemberByte' #-}
findNonMemberByte' :: Int# -> Int# -> ByteArray# -> ByteArray# -> Maybe# (# Int# , Word# #)
findNonMemberByte' off# len# set# arr# = findTemplateMemberByte' not off# len# set# arr#

{-# INLINE findTemplateMemberByte' #-}
findTemplateMemberByte' :: (Bool -> Bool) -> Int# -> Int# -> ByteArray# -> ByteArray# -> Maybe# (# Int# , Word# #)
findTemplateMemberByte' tweak off# len# set# arr# = go off
  where
  !off = I# off#
  !len = I# len#
  !end = off + len
  !set = ByteSet.ByteSet (ByteArray set#)
  !arr = ByteArray arr#
  go :: Int -> Maybe# (# Int# , Word# #)
  go !ix = if ix < end
    then 
      let !w@(W8# w#) = unsafeIndex arr ix
       in if tweak (ByteSet.member w set)
            then (# | (# unboxInt ix, w# #) #)
            else go (ix + 1)
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

findNonAsciiSpace' :: Int -> Int -> ByteArray -> Maybe# Int#
findNonAsciiSpace' !start !len !arr = go start (start + len) where
  go :: Int -> Int -> Maybe# Int#
  go !ix !end = if ix < end
    then if isAsciiSpace (PM.indexByteArray arr ix)
      then go (ix + 1) end
      else (# | unboxInt ix #)
    else (# (# #) | #)

isAsciiSpace :: Word8 -> Bool
isAsciiSpace w = w == 32 || w - 9 <= 4

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

-- | The number generated by this hashing algorithm is intended to be
--   used with division, not modulus.
hash ::
     Int -- ^ offset
  -> Int -- ^ length
  -> Int -- ^ buckets
  -> ByteArray -- ^ bytes
  -> Int
hash !off !len = hashWith off len 0

hashWith ::
     Int -- ^ offset
  -> Int -- ^ length
  -> Int -- ^ salt
  -> Int -- ^ buckets
  -> ByteArray -- ^ bytes
  -> Int
hashWith !off !len (I# salt) !buckets !arr =
  divBase2DivisorBackwards
    (hashFinalize (hashResumable off len (# salt, 0#, 0#, 0## #) arr))
    buckets

hashFinalize ::
     (# Int#, Int#, Int#, Word# #) -- ^ leftovers
  -> Int
hashFinalize (# !pos, !needed, !h, !w #) = -- error ("leftover artifact: " ++ show (W# w))
  (I# h) +
  (((I# (word2Int# w))) * indexEntropy (I# pos)) +
  ((I# pos + I# needed) * (indexEntropy (((I# pos) + 1) * bytesInWord + I# needed)))
  -- (I# h) + ((((I# pos) * bytesInWord + I# needed) + (I# (word2Int# w))) * indexEntropy (I# pos))
  -- I# h + I# needed + I# pos + I# (word2Int# w)

hashResumable ::
     Int -- ^ offset
  -> Int -- ^ length
  -> (# Int#, Int#, Int#, Word# #) -- ^ leftovers
  -> ByteArray -- ^ bytes
  -> (# Int#, Int#, Int#, Word# #)
hashResumable !off !len !leftovers !arr = case isBigEndian of
  True -> hashResumableBigEndian off len leftovers arr
  False -> hashResumableLittleEndian off len leftovers arr

hashResumableBigEndian ::
     Int -- ^ offset
  -> Int -- ^ length
  -> (# Int#, Int#, Int#, Word# #) -- ^ leftovers
  -> ByteArray -- ^ bytes
  -> (# Int#, Int#, Int#, Word# #)
hashResumableBigEndian !off !len !leftovers !arr =
  hashResumableInternal off len unsafeIndexWord leftovers arr

hashResumableLittleEndian ::
     Int -- ^ offset
  -> Int -- ^ length
  -> (# Int#, Int#, Int#, Word# #) -- ^ leftovers
  -> ByteArray -- ^ bytes
  -> (# Int#, Int#, Int#, Word# #)
hashResumableLittleEndian !off !len !leftovers !arr =
  hashResumableInternal off len unsafeIndexWordLE leftovers arr

unsafeIndexWordLE :: ByteArray -> Int -> Word
unsafeIndexWordLE arr ix = 
  let !(W# w) = unsafeIndexWord arr ix
   in W# (byteSwap# w)

{-# INLINE hashResumableInternal #-}
hashResumableInternal ::
     Int -- ^ offset
  -> Int -- ^ length
  -> (ByteArray -> Int -> Word) -- ^ big endian index into bytearray
  -> (# Int#, Int#, Int#, Word# #) -- ^ leftovers
  -> ByteArray -- ^ bytes
  -> (# Int#, Int#, Int#, Word# #)
hashResumableInternal !off !len0 normalizedIndexWord (# pos0# ,needed0#, acc0#, artifact0# #) !arr
  -- Note that pos refers to the number of bytes processed so far,
  -- while ix and off refer to a position in the current chunk. The value
  -- of needed is bounded by [0,WORD_SIZE)
  | len0 == 0 = -- empty byte array
      (# pos0# ,needed0#, acc0#, artifact0# #) 
  | machEnd == machStart = -- the byte array does not even strattle a word boundary
      if needed0 >= len0
        then
          let !x = normalizedIndexWord arr machStart
              !emptyRightBytes = bytesInWord - (end - (bytesInWord * machEnd))
              !y = unsafeShiftR x (emptyRightBytes * bytesInWord) .&. rightwardOnes (bytesInWord * len0)
              !adjustedArtifact = unsafeShiftL artifact0 (bytesInWord * len0) .|. y
           in -- error ("dieing here 1. Values: " ++ show emptyRightBytes ++ " " ++ show y ++ " " ++ show x ++ " " ++ show adjustedArtifact)
              (# pos0#, unboxInt (needed0 - len0), acc0#, unboxWord adjustedArtifact #)
        else
          let !x = normalizedIndexWord arr machStart
              !emptyRightBytes = bytesInWord - (end - (bytesInWord * machEnd))
              !xAdjusted = unsafeShiftR x (emptyRightBytes * 8) .&. rightwardOnes (8 * len0)
              !xLeft = unsafeShiftR xAdjusted ((len0 - needed0) * 8)
              !xRight = xAdjusted -- no need to mask it since that happens later
              !fullArtifact = unsafeShiftL artifact0 (8 * needed0) .|. xLeft
              !acc = acc0 + (wordToInt fullArtifact * indexEntropy pos0)
           in -- error ("dieing here 2: values " ++ show x ++ " " ++ show xRight ++ " " ++ show xLeft ++ " " ++ show xAdjusted ++ " " ++ show emptyRightBytes)
              (# pos0# +# 1#, unboxInt (bytesInWord - (len0 - needed0)), unboxInt acc, unboxWord xRight #)
  | otherwise =
      let !availableBytes = ((machStart + 1) * bytesInWord) - off
          !x = normalizedIndexWord arr machStart
       in if needed0 >= availableBytes
            then
              let !y = x .&. rightwardOnes (bytesInWord * availableBytes)
                  !adjustedArtifact = unsafeShiftL artifact0 (8 * availableBytes) .|. y
               in error "dieing here 3"
                  -- goMach adjustedArtifact acc0 (needed0 - availableBytes) (machStart + 1) pos0
            else
              let !xAdjusted = x .&. (if availableBytes == 8 then maxBound else rightwardOnes (bytesInWord * availableBytes))
                  !neededBits = needed0 * 8
                  !availableBits = availableBytes * 8
                  !xLeft = rightwardOnes neededBits .&. unsafeShiftR xAdjusted (availableBits - neededBits)
                  !xRight = xAdjusted -- no need to mask it since that happens later
                  !fullArtifact = unsafeShiftL artifact0 neededBits .|. xLeft
                  !acc = acc0 + (wordToInt fullArtifact * indexEntropy pos0)
               in -- error ("dieing here 4: " ++ show xLeft ++ " " ++ show xRight ++ " " ++ show fullArtifact ++ " " ++ show x)
                  goMach xRight acc (bytesInWord - (availableBytes - needed0)) (machStart + 1) (pos0 + 1)
  where
  !pos0 = I# pos0#
  !acc0 = I# acc0#
  !needed0 = I# needed0#
  !artifact0 = W# artifact0#
  !end = len0 + off
  !machStart = quot off bytesInWord
  !machEnd = quot end bytesInWord
  goMach :: Word -> Int -> Int -> Int -> Int -> (# Int#, Int#, Int#, Word# #)
  goMach !artifact !acc !needed !machIx !pos = if machIx < machEnd
    then 
      let !x = normalizedIndexWord arr machIx
          !neededBits = needed * 8
          !complete = unsafeShiftL artifact neededBits .|. (rightwardOnes neededBits .&. (unsafeShiftR x (bitsInWord - neededBits)))
          !newAcc = acc + (wordToInt complete * indexEntropy pos)
       in goMach x newAcc needed (machIx + 1) (pos + 1)
    else
      let !ix = machIx * bytesInWord
       in if end == ix -- we do not read past the end if the end is machine-word-aligned
            then 
              let !truncatedArtifact = (if needed == 0 then maxBound else rightwardOnes (bitsInWord - (8 * needed))) .&. artifact
               in -- error ("goMach: dieing on aligned case: " ++ show truncatedArtifact)
                  (# unboxInt pos, unboxInt needed, unboxInt acc, unboxWord truncatedArtifact #)
            else 
              let !availableBytes = end - ix
                  !x = normalizedIndexWord arr machEnd
                  !y = unsafeShiftR x (bitsInWord - (availableBytes * 8))
               in if needed >= availableBytes
                    then 
                      let !newNeeded = needed - availableBytes
                          !adjustedArtifact = (if newNeeded == 0 then maxBound else rightwardOnes (bitsInWord - 8 * newNeeded)) .&. (unsafeShiftL artifact (8 * availableBytes) .|. y)
                       in -- error ("dieing here 5: " ++ show newNeeded)
                          (# unboxInt pos, unboxInt newNeeded, unboxInt acc, unboxWord adjustedArtifact #)
                    else 
                      let !leftoverBits = (availableBytes - needed) * 8
                          !yLeft = unsafeShiftR y leftoverBits
                          !yRight = if leftoverBits == bitsInWord then y else rightwardOnes leftoverBits .&. y 
                          !fullArtifact = unsafeShiftL artifact (8 * needed) .|. yLeft
                          !acc1 = acc + (wordToInt fullArtifact * indexEntropy pos)
                       in (# unboxInt (pos + 1), unboxInt (bytesInWord - (availableBytes - needed)), unboxInt acc1, unboxWord yRight #)

bitsInWord :: Int
bitsInWord = finiteBitSize (undefined :: Word)

bytesInWord :: Int
bytesInWord = PM.sizeOf (undefined :: Word)


-- divideEight :: Int -> Int
-- divideEight x = unsafeShiftR x 3

-- A simple runtime test to determine the endianness of the
-- current platform. We assume that there are only big and
-- little endian architectures, no mixed endian.
isBigEndian :: Bool
isBigEndian = runST $ do
  m <- PM.newByteArray 8
  PM.writeByteArray m 0 (0xFF :: Word)
  w <- PM.readByteArray m 7
  return ((w :: Word8) == 0xFF)

wordToInt :: Word -> Int
wordToInt (W# w) = I# (word2Int# w)

intToWord :: Int -> Word
intToWord (I# i) = W# (int2Word# i)

-- must be a power of two
entropyCount :: Int
entropyCount = 64

indexEntropy :: Int -> Int
indexEntropy !ix = PM.indexByteArray entropy ((entropyCount - 1) .&. ix)

rightwardOnes :: Int -> Word
rightwardOnes !n = unsafeShiftL 1 n - 1

divBase2DivisorBackwards :: Int -> Int -> Int
divBase2DivisorBackwards (I# quotient) !divisor =
  wordToInt (unsafeShiftR (W# (int2Word# quotient)) (finiteBitSize (undefined :: Int) - truncLogBaseTwo divisor))

truncLogBaseTwo :: Int -> Int
truncLogBaseTwo !n = finiteBitSize (undefined :: Int) - countLeadingZeros n - 1


-- These must all be odd numbers. These were generated randomly.
entropy :: ByteArray
entropy = runST $ do
  m <- PM.newByteArray (PM.sizeOf (undefined :: Word) * entropyCount)
  PM.writeByteArray m 0 (0x63D673ED5520BA57 :: Word)
  PM.writeByteArray m 1 (0x1BD3DD04640C89E7 :: Word)
  PM.writeByteArray m 2 (6310923555544635067 :: Word)
  PM.writeByteArray m 3 (3973807439685559157 :: Word)
  PM.writeByteArray m 4 (5705095456520067183 :: Word)
  PM.writeByteArray m 5 (17211710136821461355 :: Word)
  PM.writeByteArray m 6 (2376629791045299279 :: Word)
  PM.writeByteArray m 7 (13844255342107024509 :: Word)
  PM.writeByteArray m 8 (8105732563521910659 :: Word)
  PM.writeByteArray m 9 (3328345347683869263 :: Word)
  PM.writeByteArray m 10 (2025396771326196395 :: Word)
  PM.writeByteArray m 11 (8695877615251000017 :: Word)
  PM.writeByteArray m 12 (8964413731492633943 :: Word)
  PM.writeByteArray m 13 (13449421079475920163 :: Word)
  PM.writeByteArray m 14 (3821811081509623085 :: Word)
  PM.writeByteArray m 15 (8419963512810155517 :: Word)
  PM.writeByteArray m 16 (9940200863537997427 :: Word)
  PM.writeByteArray m 17 (6533990359009873091 :: Word)
  PM.writeByteArray m 18 (11280500408256267617 :: Word)
  PM.writeByteArray m 19 (8049166832523271311 :: Word)
  PM.writeByteArray m 20 (17172297100006479811 :: Word)
  PM.writeByteArray m 21 (143326536574493877 :: Word)
  PM.writeByteArray m 22 (6114048204379035183 :: Word)
  PM.writeByteArray m 23 (2062534466603314243 :: Word)
  PM.writeByteArray m 24 (5855932815128068487 :: Word)
  PM.writeByteArray m 25 (13039379742431507963 :: Word)
  PM.writeByteArray m 26 (16313462579378763245 :: Word)
  PM.writeByteArray m 27 (11167449627757254139 :: Word)
  PM.writeByteArray m 28 (16911458830843305159 :: Word)
  PM.writeByteArray m 29 (17253354353552921593 :: Word)
  PM.writeByteArray m 30 (1077702315143872395 :: Word)
  PM.writeByteArray m 31 (16765544009052968283 :: Word)
  PM.writeByteArray m 32 (0xFBF462567AD91891 :: Word)
  PM.writeByteArray m 33 (0xEFC97A3A89B8E247 :: Word)
  PM.writeByteArray m 34 (0x4716150CED832645 :: Word)
  PM.writeByteArray m 35 (0x37413AE2C51F3D0D :: Word)
  PM.writeByteArray m 36 (0x9310AB0C19F79717 :: Word)
  PM.writeByteArray m 37 (0x473928E1CC0797C5 :: Word)
  PM.writeByteArray m 38 (0x836081F51420A91B :: Word)
  PM.writeByteArray m 39 (0x7BBD6FE5F3E71E4B :: Word)
  PM.writeByteArray m 40 (0xA71AD6B2AB4494D7 :: Word)
  PM.writeByteArray m 41 (0x4360D11122356043 :: Word)
  PM.writeByteArray m 42 (0xF9BC51F01614E2D3 :: Word)
  PM.writeByteArray m 43 (0x691DFE3BEA9654FF :: Word)
  PM.writeByteArray m 44 (0xC16B010705C887B7 :: Word)
  PM.writeByteArray m 45 (0x89DA5D8DA91EE3F5 :: Word)
  PM.writeByteArray m 46 (0x29393519EF163A6C :: Word)
  PM.writeByteArray m 47 (0xAB379EF864573EC9 :: Word)
  PM.writeByteArray m 48 (0xBFD293D697C6948E :: Word)
  PM.writeByteArray m 49 (0x45A28D8CFD308DA3 :: Word)
  PM.writeByteArray m 50 (0x7DE18C37CB04767D :: Word)
  PM.writeByteArray m 51 (0x7DFCDD66546FFDD3 :: Word)
  PM.writeByteArray m 52 (0xB99E08679B817EC3 :: Word)
  PM.writeByteArray m 53 (0x5550F6746494B1D7 :: Word)
  PM.writeByteArray m 54 (0x6533D204C643F9D3 :: Word)
  PM.writeByteArray m 55 (0x73966731E4B09DD7 :: Word)
  PM.writeByteArray m 56 (0x65DB2AC5C4779CE1 :: Word)
  PM.writeByteArray m 57 (0xC514A98EBB314AED :: Word)
  PM.writeByteArray m 58 (0x170CD919FCD5F5BA :: Word)
  PM.writeByteArray m 59 (0xF7336ADCCB5E5E8F :: Word)
  PM.writeByteArray m 60 (0xCBE7AB987C651FF5 :: Word)
  PM.writeByteArray m 61 (0x0B6906CECC7BC03E :: Word)
  PM.writeByteArray m 62 (0x9BB442B09CAC93C5 :: Word)
  PM.writeByteArray m 63 (0x196B1122B1EC700F :: Word)
  PM.unsafeFreezeByteArray m

equals ::
     Int -- offset one
  -> Int -- offset two
  -> Int -- length
  -> ByteArray -- array one
  -> ByteArray -- array two
  -> Bool
equals !off1 !off2 !len !arr1 !arr2 = if len > 0
  then unsafeIndex arr1 off1 == unsafeIndex arr2 off2
    && equals (off1 + 1) (off2 + 1) (len - 1) arr1 arr2
  else True

-- | The returned sum has three options:
--
--   * Did not match.
--   * Did match.
--   * Out of input.
stripPrefixResumable ::
     Int -- ^ prefix offset
  -> Int -- ^ offset
  -> Int -- ^ prefix length
  -> Int -- ^ length
  -> ByteArray -- ^ prefix array
  -> ByteArray -- ^ array
  -> (# (# #) | (# #) | (# #) #)
stripPrefixResumable !poff !off !plen !len !parr !arr = if plen > len
  then case equals poff off len parr arr of
    True -> (# | | (# #) #)
    False -> (# (# #) | | #)
  else case equals poff off plen parr arr of
    True -> (# | (# #) | #)
    False -> (# (# #) | | #)


