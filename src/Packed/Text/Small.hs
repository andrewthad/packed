{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC
 -Weverything
 -fno-warn-unsafe
 -fno-warn-implicit-prelude
 -fno-warn-missing-import-lists
 -O2
#-}
module Packed.Text.Small
  ( SmallText(..)
  , empty
  , singleton
  , decodeAscii
  , encodeUtf8
  , pack
  , unpack
  , intercalate
  ) where

import Prelude hiding (reverse)

import Packed.Bytes.Small (ByteArray)
import Data.Semigroup (Semigroup)
import Data.Primitive (PrimUnlifted,UnliftedArray)
import Data.String (IsString(fromString))
import Control.Monad.ST (runST)
import qualified Data.Primitive as PM
import qualified Packed.Bytes.Small as BA
import qualified Packed.Text.Window as TW

newtype SmallText = SmallText { getSmallText :: ByteArray }
  deriving (Eq,Ord,Semigroup,Monoid,PrimUnlifted)

instance Show SmallText where
  showsPrec p ps r = showsPrec p (unpack ps) r

instance IsString SmallText where
  fromString = pack

-- Text is UTF-8 encoded. Unlike the type used for sliced text,
-- this type does not have any way to track whether or not any non-ascii
-- characters are present in the text. The functions take, drop, reverse,
-- and index do not get the same speed ups they do for the sliced variant.

empty :: SmallText
empty = SmallText BA.empty

singleton :: Char -> SmallText
singleton = SmallText . TW.singleton

decodeAscii :: ByteArray -> Maybe SmallText
decodeAscii arr = if BA.isAscii arr
  then Just (SmallText arr)
  else Nothing

encodeUtf8 :: SmallText -> ByteArray
encodeUtf8 (SmallText t) = t

unpack :: SmallText -> String
unpack (SmallText t) = TW.unpack 0 (BA.length t) t

pack :: String -> SmallText
pack s = let (arr,_) = TW.pack s in SmallText arr

intercalate :: SmallText -> UnliftedArray SmallText -> SmallText
intercalate (SmallText sep) pieces
  | sz < 1 = empty
  | otherwise = runST $ do
      let byteTotal = ((sz - 1) * BA.length sep) + sumByteLengths pieces
          sepSz = PM.sizeofByteArray sep
      marr <- PM.newByteArray byteTotal
      let SmallText piece0 = PM.indexUnliftedArray pieces 0
      PM.copyByteArray marr 0 piece0 0 (PM.sizeofByteArray piece0)
      !_ <- headlessFoldlUnliftedArrayM'
        (\ix (SmallText arr) -> do
          PM.copyByteArray marr ix sep 0 sepSz
          let arrSz = PM.sizeofByteArray arr
          PM.copyByteArray marr (ix + sepSz) arr 0 arrSz
          return (ix + sepSz + arrSz)
        ) (PM.sizeofByteArray piece0) pieces
      arr <- PM.unsafeFreezeByteArray marr 
      return (SmallText arr)
  where
  sz = PM.sizeofUnliftedArray pieces

sumByteLengths :: UnliftedArray SmallText -> Int
sumByteLengths = PM.foldlUnliftedArray' (\n (SmallText arr) -> BA.length arr + n) 0

-- A left monadic fold that skips the first element in the array
headlessFoldlUnliftedArrayM' :: (PrimUnlifted a, Monad m) => (b -> a -> m b) -> b -> UnliftedArray a -> m b
headlessFoldlUnliftedArrayM' f z0 arr = go 1 z0
  where
    !sz = PM.sizeofUnliftedArray arr
    go !i !acc1
      | i < sz = do
          acc2 <- f acc1 (PM.indexUnliftedArray arr i)
          go (i + 1) acc2
      | otherwise = return acc1
