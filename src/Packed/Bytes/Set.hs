{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC
 -Weverything
 -fno-warn-unsafe
 -fno-warn-implicit-prelude
 -fno-warn-missing-import-lists
 -O2
#-}


module Packed.Bytes.Set
  ( ByteSet(..)
  , member
  , fromList
  , invert
  ) where

import Data.Primitive (ByteArray)
import Data.Semigroup (Semigroup)
import Control.Monad.ST (ST,runST)
import Data.Word (Word8)
import Data.Bits ((.|.),complement)
import Control.Monad (forM_)
import qualified Data.Primitive as PM
import qualified Data.Semigroup as SG

data ByteSet = ByteSet {-# UNPACK #-} !ByteArray

instance Semigroup ByteSet where
  (<>) = append

instance Monoid ByteSet where
  mappend = (SG.<>)
  mempty = empty

append :: ByteSet -> ByteSet -> ByteSet
append (ByteSet a) (ByteSet b) = ByteSet (zipOrMach 256 a b)

empty :: ByteSet
empty = ByteSet $ runST $ do
  marr <- PM.newByteArray 256
  PM.fillByteArray marr 0 256 0
  PM.unsafeFreezeByteArray marr

fromList :: [Word8] -> ByteSet
fromList ws = runST $ do
  marr <- PM.newByteArray 256
  PM.fillByteArray marr 0 256 0
  forM_ ws $ \w -> do
    PM.writeByteArray marr (word8ToInt w) (0xFF :: Word8)
  arr <- PM.unsafeFreezeByteArray marr
  return (ByteSet arr)

invert :: ByteSet -> ByteSet
invert (ByteSet arr) = runST $ do
  marr <- PM.newByteArray 256
  let go !ix = if ix < quot 256 (PM.sizeOf (undefined :: Word))
        then do
          PM.writeByteArray marr ix (complement (PM.indexByteArray arr ix :: Word))
          go (ix + 1)
        else return ()
  go 0
  newArr <- PM.unsafeFreezeByteArray marr
  return (ByteSet newArr)

word8ToInt :: Word8 -> Int
word8ToInt = fromIntegral

member :: Word8 -> ByteSet -> Bool
member !w (ByteSet arr) = PM.indexByteArray arr (word8ToInt w) == (0xFF :: Word8)

-- The machine word size must divide into the length evenly, but this is
-- unchecked.
zipOrMach ::
     Int -- len
  -> ByteArray -- x
  -> ByteArray -- y
  -> ByteArray -- z
zipOrMach !len !x !y = runST action
  where
  action :: forall s. ST s ByteArray
  action = do
    let !lenMach = quot len (PM.sizeOf (undefined :: Word))
    !marr <- PM.newByteArray len
    let goMach :: Int -> ST s ()
        goMach !ix = if ix < lenMach
          then do
            PM.writeByteArray marr ix (unsafeIndexWord x ix .|. unsafeIndexWord y ix)
            goMach (ix + 1)
          else return ()
    goMach 0 
    PM.unsafeFreezeByteArray marr

-- this is only used internally
unsafeIndexWord :: ByteArray -> Int -> Word
unsafeIndexWord = PM.indexByteArray

