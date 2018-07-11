{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Data.ByteMap
  ( ByteMap
  , empty
  , singleton
  , doubleton
  , lookup
  , insert
  , insertWith
  , appendWith
  , toList
  , fromList
  , foldrWithKey
  , foldMapWithKey
  ) where

import Prelude hiding (foldMap,lookup)

import Control.Monad.ST (runST,ST)
import Data.Primitive hiding (fromList)
import Data.Semigroup (Semigroup(..))
import GHC.Exts (Int(I#),int2Word#,IsList)
import GHC.Word (Word8(W8#))

import qualified Data.Foldable as F
import qualified GHC.Exts

-- Invariants: The values in the PrimArray are indices into the
-- SmallArray. To signify that something does not exist, use
-- 255. This will mean that there is no value for this entry
-- unless the length of the SmallArray is 256 (meaning that
-- there is a value for every entry). In such a case, the start
-- value must be zero.
--
-- When the length of the values is 256, we are in somewhat
-- special case. This tells us that the values from the
-- PrimArray are equal to the indexes at which they are located.
data ByteMap a = ByteMap
  !Int -- start, must be less than 256
  !(PrimArray Word8) -- array (also used for length)
  !(SmallArray a)
  deriving (Eq,Ord,Functor)

data MutableByteMap s a = MutableByteMap
  !Int
  !(MutablePrimArray s Word8)
  !(SmallMutableArray s a)

instance IsList (ByteMap a) where
  type Item (ByteMap a) = (Word8,a)
  fromList = fromList
  fromListN _ = fromList
  toList = toList

instance Foldable ByteMap where
  foldMap = foldMap

instance Semigroup a => Semigroup (ByteMap a) where
  (<>) = appendWith (<>)
  stimes n = fmap (stimes n)

instance Semigroup a => Monoid (ByteMap a) where
  mempty = empty

instance Show a => Show (ByteMap a) where
  showsPrec p xs = showParen (p > 10) $
    showString "fromList " . shows (toList xs)

lookup :: Word8 -> ByteMap a -> Maybe a
lookup w (ByteMap start ixs vals) = case sizeofSmallArray vals of
  256 -> Just (indexSmallArray vals (word8ToInt w))
  _ -> if word8ToInt w < start || word8ToInt w >= start + sizeofPrimArray ixs
    then Nothing
    else case indexPrimArray ixs (word8ToInt w - start) of
      255 -> Nothing
      ix -> Just (indexSmallArray vals (word8ToInt ix))

fromList :: [(Word8,a)] -> ByteMap a
fromList zs = runST $ do
  let go [] m = pure m
      go ((k,v):xs) m = insertMutableByteMapWith const m k v >>= go xs
  unsafeFreezeMutableByteMap =<< go zs =<< newMutableByteMap

toList :: ByteMap a -> [(Word8,a)]
toList = foldrWithKey (\k v xs -> (k,v) : xs) []

empty :: ByteMap a
empty = runST (newMutableByteMap >>= unsafeFreezeMutableByteMap)

newMutableByteMap :: ST s (MutableByteMap s a)
newMutableByteMap = do
  ixs <- newPrimArray 0
  vals <- thawSmallArray mempty 0 0
  pure (MutableByteMap 0 ixs vals)

-- This implementation is terrible. Efficient construction is not
-- a huge goal of ByteMap, but this is currently asymptotically
-- worse than it should be.
appendWith :: (a -> a -> a) -> ByteMap a -> ByteMap a -> ByteMap a
appendWith f = foldrWithKey (\rk rv m -> insertWith f rk rv m)

mutableSingleton :: Word8 -> a -> ST s (MutableByteMap s a)
mutableSingleton k v = do
  mutIxs <- newPrimArray 1
  writePrimArray mutIxs 0 (0 :: Word8)
  mutVals <- newSmallArray 1 v
  return (MutableByteMap (word8ToInt k) mutIxs mutVals)

singleton :: Word8 -> a -> ByteMap a
singleton k v = runST (mutableSingleton k v >>= unsafeFreezeMutableByteMap)

insertWith :: (a -> a -> a) -> Word8 -> a -> ByteMap a -> ByteMap a
insertWith f k v bm = runST $ do
  mbm0 <- thawByteMap bm
  mbm1 <- insertMutableByteMapWith f mbm0 k v
  unsafeFreezeMutableByteMap mbm1

insert :: Word8 -> a -> ByteMap a -> ByteMap a
insert = insertWith const

-- This replaces the existing entry. Do not reuse the argument map
-- after passing it to this function.
insertMutableByteMapWith :: (a -> a -> a) -> MutableByteMap s a -> Word8 -> a -> ST s (MutableByteMap s a)
insertMutableByteMapWith f (MutableByteMap start ixs vals) k v = do
  sz <- getSizeofMutablePrimArray ixs
  let szVals = sizeofSmallMutableArray vals
  if | szVals == 256 -> do
         v0 <- readSmallArray vals (word8ToInt k)
         writeSmallArray vals (word8ToInt k) (f v v0)
         return (MutableByteMap start ixs vals)
     | szVals == 0 -> mutableSingleton k v
     | word8ToInt k < start -> do
         let newIxsSize = start + sz - word8ToInt k
         newIxs <- newPrimArray newIxsSize
         writePrimArray newIxs 0 (0 :: Word8)
         setPrimArray newIxs 1 (start - word8ToInt k - 1) 255
         copyIncrementShift newIxs (start - word8ToInt k) ixs 0 sz
         newVals <- newSmallArray (szVals + 1) v
         copySmallMutableArray newVals 1 vals 0 szVals 
         return (MutableByteMap (word8ToInt k) newIxs newVals)
     | word8ToInt k < start + sz -> do
         ix <- readPrimArray ixs (word8ToInt k - start) 
         if ix == 255
           then do
             presentCountPost <- incrementIndicesFrom ixs (word8ToInt k - start + 1)
             newVals <- newSmallArray (szVals + 1) v
             let presentCountPre = szVals - presentCountPost
             copySmallMutableArray newVals 0 vals 0 presentCountPre
             copySmallMutableArray newVals (presentCountPre + 1) vals presentCountPre presentCountPost
             -- writeSmallArray newVals presentCountPre v
             writePrimArray ixs (word8ToInt k - start) (intToWord8 presentCountPre)
             return (MutableByteMap start ixs newVals)
           else do
             v0 <- readSmallArray vals (word8ToInt ix)
             writeSmallArray vals (word8ToInt ix) (f v v0)
             return (MutableByteMap start ixs vals)
     | otherwise -> do
         let range = word8ToInt k - start
         -- error ("failing now with key " ++ show k ++ " and size " ++ show sz ++ " and range " ++ show range)
         newIxs <- newPrimArray (range + 1)
         copyMutablePrimArray newIxs 0 ixs 0 sz
         setPrimArray newIxs sz (range - sz) (255 :: Word8)
         writePrimArray newIxs range (intToWord8 szVals)
         newVals <- newSmallArray (szVals + 1) v
         copySmallMutableArray newVals 0 vals 0 szVals 
         return (MutableByteMap start newIxs newVals)

thawByteMap :: ByteMap a -> ST s (MutableByteMap s a)
thawByteMap (ByteMap start ixs vals) = do
  let sz = sizeofPrimArray ixs
  mutIxs <- newPrimArray sz
  copyPrimArray mutIxs 0 ixs 0 sz
  mutVals <- thawSmallArray vals 0 (sizeofSmallArray vals)
  return (MutableByteMap start mutIxs mutVals)

unsafeFreezeMutableByteMap :: MutableByteMap s a -> ST s (ByteMap a)
unsafeFreezeMutableByteMap (MutableByteMap start mutIxs mutVals) = do
  ixs <- unsafeFreezePrimArray mutIxs
  vals <- unsafeFreezeSmallArray mutVals
  return (ByteMap start ixs vals)

copyIncrementShift ::
     MutablePrimArray s Word8 -- destination
  -> Int -- destination offset
  -> MutablePrimArray s Word8 -- source
  -> Int -- source offset
  -> Int -- size
  -> ST s ()
copyIncrementShift dst dstOff src srcOff sz = go dstOff srcOff where
  go dstIx srcIx = if srcIx < srcOff + sz
    then do
      v <- readPrimArray src srcIx
      let w = if v == 255 then v else v + 1
      writePrimArray dst dstIx w
      go (dstIx + 1) (srcIx + 1)
    else pure ()
  

incrementIndicesFrom ::
     MutablePrimArray s Word8
  -> Int -- start position to increment indices
  -> ST s Int -- return the total number of non-255 values
incrementIndicesFrom marr start = do
  sz <- getSizeofMutablePrimArray marr
  let go ix ct = if ix < sz
        then do
          v <- readPrimArray marr ix
          if v == 255
            then go (ix + 1) ct
            else do
              writePrimArray marr ix (v + 1)
              go (ix + 1) (ct + 1)
        else pure ct
  go start 0

foldMap :: Monoid m => (a -> m) -> ByteMap a -> m
foldMap f (ByteMap _ _ vals) = F.foldMap f vals

foldMapWithKey :: Monoid m => (Word8 -> a -> m) -> ByteMap a -> m
foldMapWithKey f (ByteMap start ixs vals) = case sizeofSmallArray vals of
  256 -> ifoldMapSmallArray f vals
  _ -> ifoldMapPrimArray (\key ix -> if ix == 255 then mempty else f (intToWord8 (word8ToInt key + start)) (indexSmallArray vals (word8ToInt ix))) ixs

foldrWithKey :: (Word8 -> a -> b -> b) -> b -> ByteMap a -> b
foldrWithKey f b0 (ByteMap start ixs vals) = case sizeofSmallArray vals of
  256 -> ifoldrSmallArray f b0 vals
  _ -> ifoldrPrimArray (\key ix b -> if ix == 255 then b else f (intToWord8 (word8ToInt key + start)) (indexSmallArray vals (word8ToInt ix)) b) b0 ixs

ifoldMapSmallArray :: Monoid m => (Word8 -> a -> m) -> SmallArray a -> m
ifoldMapSmallArray f arr = go 0 where
  go !ix = if ix < sizeofSmallArray arr
    then mappend (f (fromIntegral ix) (indexSmallArray arr ix)) (go (ix + 1))
    else mempty

ifoldMapPrimArray :: (Monoid m, Prim a) => (Word8 -> a -> m) -> PrimArray a -> m
ifoldMapPrimArray f arr = go 0 where
  go !ix = if ix < sizeofPrimArray arr
    then mappend (f (fromIntegral ix) (indexPrimArray arr ix)) (go (ix + 1))
    else mempty

ifoldrPrimArray :: Prim a => (Word8 -> a -> b -> b) -> b -> PrimArray a -> b
ifoldrPrimArray f b0 arr = go 0 where
  go !ix = if ix < sizeofPrimArray arr
    then f (fromIntegral ix) (indexPrimArray arr ix) (go (ix + 1))
    else b0

ifoldrSmallArray :: (Word8 -> a -> b -> b) -> b -> SmallArray a -> b
ifoldrSmallArray f b0 arr = go 0 where
  go !ix = if ix < sizeofSmallArray arr
    then f (fromIntegral ix) (indexSmallArray arr ix) (go (ix + 1))
    else b0

foldMapPrimArray :: (Prim a, Monoid m) => (a -> m) -> PrimArray a -> m
foldMapPrimArray f = foldrPrimArray (\a m -> f a <> m) mempty

-- the two keys must differ
doubleton ::
     Word8
  -> a
  -> Word8
  -> a
  -> ByteMap a
doubleton !k1 !v1 !k2 !v2 = runST $ do
  let lo = min k1 k2
      hi = max k1 k2
      loVal = if lo == k1 then v1 else v2
      hiVal = if lo == k1 then v2 else v1
      range = word8ToInt hi - word8ToInt lo
  mutVals <- newSmallArray 2 loVal
  writeSmallArray mutVals 1 hiVal
  vals <- unsafeFreezeSmallArray mutVals
  mutIxs <- newPrimArray (range + 1)
  setPrimArray mutIxs 1 (range - 1) (255 :: Word8)
  writePrimArray mutIxs 0 (0 :: Word8)
  writePrimArray mutIxs range (1 :: Word8)
  ixs <- unsafeFreezePrimArray mutIxs
  return (ByteMap (word8ToInt lo) ixs vals)

word8ToInt :: Word8 -> Int
word8ToInt = fromIntegral

-- precondition: the int must be between 0 and 255
intToWord8 :: Int -> Word8
intToWord8 (I# i) = W8# (int2Word# i)
