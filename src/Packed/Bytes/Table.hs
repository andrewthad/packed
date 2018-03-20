{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC
 -Weverything
 -fno-warn-unsafe
 -fno-warn-implicit-prelude
 -fno-warn-missing-import-lists
 -O2
#-}

module Packed.Bytes.Table
  ( BytesTable
  , lookup
  , fromList
  , fromListWith
  , construct
  , twoExp
  , truncLogBaseTwo
  , remBase2Divisor
  , buildCollisionless
  , ArrList(..)
  ) where

import Prelude hiding (lookup)
import Data.Primitive (ByteArray,MutableArray)
import Data.Primitive.SmallArray (SmallArray)
import Packed.Bytes (Bytes)
import Control.Monad.ST (ST,runST)
import Data.Monoid (Any(..))
import Data.Bits ((.&.),unsafeShiftL,countLeadingZeros,finiteBitSize)

import qualified Packed.Bytes.Small as SB
import qualified Packed.Bytes as B
import qualified Data.Primitive as PM
import qualified Data.Primitive.SmallArray as PMSA

-- | A static perfect hash table where the keys are byte arrays. This
--   table cannot be updated after its creation, but all lookups have
--   guaranteed O(1) worst-case cost. It consumes linear space. This
--   is an excellent candidate for use with compact regions.
newtype BytesTable v = BytesTable (SmallArray (Cell v))
-- invariant for BytesTable: the Array must have a length
-- that is a power of two.

data Cell v
  = CellZero
  | CellOne
      {-# UNPACK #-} !ByteArray -- payload
      !v -- value
  | CellMany
      {-# UNPACK #-} !Int -- hash salt
      {-# UNPACK #-} !(SmallArray (Info v)) -- length must be power of two

data Info v
  = InfoAbsent
  | InfoPresent
      {-# UNPACK #-} !ByteArray --payload
      !v

-- data ByteArrayArray = ByteArrayArray ArrayArray#

data TableBuilder s v = TableBuilder
  {-# UNPACK #-} !Int -- count of the total number of distinct ByteArrays
  {-# UNPACK #-} !(MutableArray s (ArrList v))

data ArrList v
  = ArrListCons !ByteArray !v !(ArrList v)
  | ArrListNil

showArrList :: ArrList v -> String
showArrList = show . arrListToList

arrListToList :: ArrList v -> [ByteArray]
arrListToList ArrListNil = []
arrListToList (ArrListCons b _ xs) = b : arrListToList xs

emptyTableBuilder :: ST s (TableBuilder s v)
emptyTableBuilder = do
  marr <- PM.newArray 8 ArrListNil
  return (TableBuilder 0 marr)

lookup :: Bytes -> BytesTable v -> Maybe v
lookup !needle (BytesTable sarrOuter) =
  let !outerHash = B.hash (PMSA.sizeofSmallArray sarrOuter) needle in
  case PMSA.indexSmallArray sarrOuter outerHash of
    CellZero -> Nothing
    CellOne ba v -> if B.equalsByteArray ba needle
      then Just v
      else Nothing
    CellMany salt sarrInner ->
      let !innerHash = B.hashWith salt (PMSA.sizeofSmallArray sarrInner) needle in
      case PMSA.indexSmallArray sarrInner innerHash of
        InfoAbsent -> Nothing
        InfoPresent ba v -> if B.equalsByteArray ba needle
          then Just v
          else Nothing

-- This calls freeze on the arrays inside of the builder,
-- so do not reuse it after calling this function.
freezeBuilder :: forall s v. TableBuilder s v -> ST s (BytesTable v)
freezeBuilder (TableBuilder _ marr) = do
  msarr <- PMSA.newSmallArray (PM.sizeofMutableArray marr) CellZero
  let go :: Int -> ST s ()
      go !ix = if ix < PM.sizeofMutableArray marr
        then do
          arrList <- PM.readArray marr ix
          case arrList of
            ArrListNil -> return () -- already been set to CellZero
            ArrListCons b v ArrListNil -> do
              PMSA.writeSmallArray msarr ix (CellOne b v)
            ArrListCons _ _ (ArrListCons _ _ _) -> do
              (salt, sarr) <- buildCollisionless 0 arrList
              PMSA.writeSmallArray msarr ix (CellMany salt sarr)
          go (ix + 1)
        else return ()
  go 0
  sarr <- PMSA.unsafeFreezeSmallArray msarr
  return (BytesTable sarr)

-- This function is not guaranteed to terminate. An attacker
-- could cause this to loop forever. The odds of this occurring
-- naturally are nearly zero. Do not pass this function an ArrList
-- of length zero.
buildCollisionless :: Int -> ArrList v -> ST s (Int,SmallArray (Info v))
buildCollisionless !salt !arrList = if salt < 30
  then do
    let !arrLen = arrListLength arrList 
        !len = twoExp (truncLogBaseTwo (arrLen * arrLen))
    msarr <- PMSA.newSmallArray len InfoAbsent
    Any hasCollisions <- arrListForM_ arrList $ \b v -> do
      let !ix = SB.hashWith salt len b
      x <- PMSA.readSmallArray msarr ix
      case x of
        InfoAbsent -> do
          PMSA.writeSmallArray msarr ix (InfoPresent b v)
          return (Any False)
        InfoPresent existing _ -> return (Any True) -- error ("buildCollisionless: found " ++ show existing ++ " at ix " ++ show ix) -- return (Any True)
    -- _ <- fail ("hasCollisions: " ++ show hasCollisions)
    if hasCollisions
      then buildCollisionless (salt + 1) arrList
      else do
        sarr <- PMSA.unsafeFreezeSmallArray msarr
        return (salt, sarr)
  else error ("buildCollisionless: too many tries: " ++ showArrList arrList ++ " length: " ++ show (arrListLength arrList))
      

construct :: forall v c.
     (v -> v -> v)
  -> (forall s d. (Bytes -> v -> c -> d -> ST s d) -> c -> d -> ST s d)
  -> c 
  -> BytesTable v
construct combine f c0 = runST $ do
  tb0 <- emptyTableBuilder
  tb <- microConstruct combine f c0 tb0
  freezeBuilder tb

microConstruct :: forall v c s.
     (v -> v -> v)
  -> ((Bytes -> v -> c -> TableBuilder s v -> ST s (TableBuilder s v)) -> c -> TableBuilder s v -> ST s (TableBuilder s v))
  -> c -> TableBuilder s v -> ST s (TableBuilder s v)
microConstruct combine f c0 tb0 = f (\b v c d -> do
    d' <- insertBuilder combine d (B.toByteArray b) v
    microConstruct combine f c d'
  ) c0 tb0

insertBuilder :: (v -> v -> v) -> TableBuilder s v -> ByteArray -> v -> ST s (TableBuilder s v)
insertBuilder combine (TableBuilder count marr0) key val = do
  marr1 <- if count < PM.sizeofMutableArray marr0
    then return marr0
    else growBuilderArray combine marr0
  insertBuilderArray combine marr1 key val
  return (TableBuilder (count + 1) marr1)
      
truncLogBaseTwo :: Int -> Int
truncLogBaseTwo n = finiteBitSize (undefined :: Int) - countLeadingZeros n - 1

twoExp :: Int -> Int
twoExp n = unsafeShiftL 1 n

growBuilderArray :: (v -> v -> v) -> MutableArray s (ArrList v) -> ST s (MutableArray s (ArrList v))
growBuilderArray combine marr = do
  marrBig <- PM.newArray (PM.sizeofMutableArray marr * 2) ArrListNil
  builderArrayForM_ marr $ \b v -> do
    -- even though we pass combine, it should actually
    -- never be used here since everything should already
    -- be unique.
    insertBuilderArray combine marrBig b v
  return marrBig
  
-- this function cannot resize the table
insertBuilderArray :: (v -> v -> v) -> MutableArray s (ArrList v) -> ByteArray -> v -> ST s ()
insertBuilderArray combine marr b v = do
  let theHash = SB.hash (PM.sizeofMutableArray marr) b 
  arrList <- PM.readArray marr theHash
  let newArrList = insertArrList combine b v arrList
  PM.writeArray marr theHash newArrList

insertArrList :: (v -> v -> v) -> ByteArray -> v -> ArrList v -> ArrList v
insertArrList _ b v ArrListNil = ArrListCons b v ArrListNil
insertArrList combine b v (ArrListCons b' v' xs) =
  if b == b'
    then ArrListCons b' (combine v v') xs
    else ArrListCons b' v' (insertArrList combine b v xs)

-- precondition: the divisor must be two raised to some power.
remBase2Divisor :: Int -> Int -> Int
remBase2Divisor quotient divisor = quotient .&. (divisor - 1)

builderArrayForM_ ::
     MutableArray s (ArrList v)
  -> (ByteArray -> v -> ST s ())
  -> ST s ()
builderArrayForM_ marr f = go (PM.sizeofMutableArray marr - 1) where
  go ix = if ix >= 0
    then do
      arrList <- PM.readArray marr ix
      arrListForM_ arrList f
      go (ix - 1)
    else return ()

-- expects a commutative monoid
arrListForM_ :: forall s a v.
     Monoid a
  => ArrList v
  -> (ByteArray -> v -> ST s a)
  -> ST s a
arrListForM_ arrList f = go arrList mempty
  where
  go :: ArrList v -> a -> ST s a
  go ArrListNil !a = return a
  go (ArrListCons b v xs) !a = do
    a' <- f b v
    go xs (mappend a a')

arrListLength :: forall v. ArrList v -> Int
arrListLength = go 0 where
  go :: Int -> ArrList v -> Int
  go !n ArrListNil = n
  go !n (ArrListCons _ _ xs) = go (n + 1) xs

fromList :: [(Bytes,v)] -> BytesTable v
fromList = fromListWith const

fromListWith :: (v -> v -> v) -> [(Bytes,v)] -> BytesTable v
fromListWith combine = construct combine (\f xs d -> case xs of
    [] -> return d
    (b,v) : ys -> f b v ys d
  )



-- lookup :: Bytes -> BytesTable -> 

