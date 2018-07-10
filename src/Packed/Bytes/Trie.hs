{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}

{-# OPTIONS_GHC -O2 -Wall #-}

module Packed.Bytes.Trie
  ( Trie(..)
  , Node(..)
  , Run(..)
  , singleton
  , toList
  , fromList
  , fromListAppend
  , lookup
  , lookupM
  -- * Internal
  , valid
  ) where

import Prelude hiding (lookup)

import Control.Monad.ST (runST,ST)
import Data.Bifunctor (second)
import Data.ByteMap (ByteMap)
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.Primitive hiding (fromList)
import Data.Semigroup (Semigroup,First(..))
import Data.Word (Word8)
import GHC.Exts (Int(I#),int2Word#)
import GHC.Word (Word8(W8#))
import Packed.Bytes (Bytes)

import qualified Data.ByteMap as BM
import qualified Data.Semigroup as SG
import qualified Packed.Bytes as B
import qualified Packed.Bytes.Small as BA

debugMode :: Bool
debugMode = False

-- | A trie for a sequence of bytes.
newtype Trie a = Trie (Node 'ChildBranch a)
  deriving (Functor)

instance Eq a => Eq (Trie a) where
  Trie x == Trie y = eqNode x y

instance Show a => Show (Trie a) where
  showsPrec p t = showParen (p > 10) (showList (toList t))

data Child = ChildRun | ChildBranch

type family AddChild (x :: Child) (y :: Child) :: Child where
  AddChild 'ChildRun 'ChildRun = 'ChildRun
  AddChild 'ChildRun 'ChildBranch = 'ChildBranch
  AddChild 'ChildBranch y = 'ChildBranch

-- Every time an array of nodes shows up in here, that array must
-- have length 256
data Node :: Child -> Type -> Type where
  NodeBranch :: {-# UNPACK #-} !(ByteMap (Node 'ChildBranch a)) -> Node c a
  NodeRun :: {-# UNPACK #-} !(Run a) -> Node 'ChildBranch a
  NodeValueNil :: !a -> Node c a
  NodeValueBranch :: !a -> {-# UNPACK #-} !(ByteMap (Node 'ChildBranch a)) -> Node c a
  NodeValueRun :: !a -> {-# UNPACK #-} !(Run a) -> Node c a
  NodeEmpty :: Node 'ChildBranch a

deriving instance Functor (Node c)

instance Eq a => Eq (Node c a) where
  (==) = eqNode

eqNode :: Eq a => Node c a -> Node c a -> Bool
eqNode = go where
  go :: Eq b => Node e b -> Node f b -> Bool
  go (NodeBranch x) (NodeBranch y) = x == y
  go (NodeRun x) (NodeRun y) = x == y
  go (NodeValueNil x) (NodeValueNil y) = x == y
  go (NodeValueBranch v1 b1) (NodeValueBranch v2 b2) = v1 == v2 && b1 == b2
  go (NodeValueRun v1 r1) (NodeValueRun v2 r2) = v1 == v2 && r1 == r2
  go NodeEmpty NodeEmpty = True
  go _ _ = False

data Run a = Run
  {-# UNPACK #-} !ByteArray -- invariant: byte array is non empty
  !(Node 'ChildRun a)
  deriving (Functor,Eq)

-- Anything that can be a child of run can be a child of
-- branch, so this is safe. This could be written with
-- unsafeCoerce instead.
coerceNode :: Node c a -> Node 'ChildBranch a
coerceNode (NodeRun r) = NodeRun r
coerceNode (NodeBranch r) = NodeBranch r
coerceNode (NodeValueNil r) = NodeValueNil r
coerceNode (NodeValueBranch v b) = NodeValueBranch v b
coerceNode (NodeValueRun v b) = NodeValueRun v b
coerceNode NodeEmpty = NodeEmpty

append :: Semigroup a => Trie a -> Trie a -> Trie a
append (Trie x) (Trie y) = Trie (appendNode x y)

appendNode :: Semigroup a => Node c a -> Node d a -> Node (AddChild c d) a
appendNode = go where
  go :: forall b (e :: Child) (f :: Child). Semigroup b => Node e b -> Node f b -> Node (AddChild e f) b
  go (NodeRun x) (NodeRun y) = case appendRuns x y of
    Left b -> NodeBranch b
    Right r -> NodeRun r
  go (NodeRun x) (NodeValueRun v y) = case appendRuns x y of
    Left b -> NodeValueBranch v b
    Right r -> NodeValueRun v r
  go (NodeRun x) NodeEmpty = NodeRun x
  go (NodeRun x) (NodeValueNil v) = NodeValueRun v x
  go (NodeRun x) (NodeBranch y) = NodeBranch (appendRunBranch x y)
  go (NodeRun x) (NodeValueBranch v y) = NodeValueBranch v (appendRunBranch x y)
  go (NodeValueRun v x) (NodeRun y) = case appendRuns x y of
    Left b -> NodeValueBranch v b
    Right r -> NodeValueRun v r
  go (NodeValueRun v1 x) (NodeValueRun v2 y) = case appendRuns x y of
    Left b -> NodeValueBranch (v1 SG.<> v2) b
    Right r -> NodeValueRun (v1 SG.<> v2) r
  go (NodeValueRun v x) NodeEmpty = NodeValueRun v x
  go (NodeValueRun v1 x) (NodeValueNil v2) = NodeValueRun (v1 SG.<> v2) x
  go (NodeValueRun v x) (NodeBranch y) = NodeValueBranch v (appendRunBranch x y)
  go (NodeValueRun v1 x) (NodeValueBranch v2 y) = NodeValueBranch (v1 SG.<> v2) (appendRunBranch x y)
  go (NodeBranch x) (NodeBranch y) = NodeBranch (appendBranches x y)
  go (NodeBranch x) (NodeValueBranch v y) = NodeValueBranch v (appendBranches x y)
  go (NodeBranch x) (NodeValueNil v) = NodeValueBranch v x
  go (NodeBranch x) NodeEmpty = NodeBranch x
  go (NodeBranch x) (NodeRun y) = NodeBranch (appendBranchRun x y)
  go (NodeBranch x) (NodeValueRun v y) = NodeValueBranch v (appendBranchRun x y)
  go (NodeValueBranch v x) NodeEmpty = NodeValueBranch v x
  go (NodeValueBranch v1 x) (NodeValueBranch v2 y) = NodeValueBranch (v1 SG.<> v2) (appendBranches x y)
  go (NodeValueBranch v1 x) (NodeValueNil v2) = NodeValueBranch (v1 SG.<> v2) x
  go (NodeValueBranch v x) (NodeBranch y) = NodeValueBranch v (appendBranches x y)
  go (NodeValueBranch v x) (NodeRun y) = NodeValueBranch v (appendBranchRun x y)
  go (NodeValueBranch v1 x) (NodeValueRun v2 y) = NodeValueBranch (v1 SG.<> v2) (appendBranchRun x y)
  go (NodeValueNil v) (NodeBranch y) = NodeValueBranch v y
  go (NodeValueNil v) (NodeRun y) = NodeValueRun v y
  go (NodeValueNil v) NodeEmpty = NodeValueNil v
  go (NodeValueNil v1) (NodeValueBranch v2 y) = NodeValueBranch (v1 SG.<> v2) y
  go (NodeValueNil v1) (NodeValueRun v2 y) = NodeValueRun (v1 SG.<> v2) y
  go (NodeValueNil x) (NodeValueNil y) = NodeValueNil (x SG.<> y)
  go NodeEmpty n = coerceNode n 

prependRun :: Semigroup a => Run a -> Node 'ChildRun a -> Node 'ChildRun a
prependRun !r1 !x = case x of
  NodeValueNil v2 -> NodeValueRun v2 r1
  NodeValueRun v2 r2 -> case appendRuns r1 r2 of
    Left br -> NodeValueBranch v2 br
    Right r -> NodeValueRun v2 r
  NodeBranch br2 -> NodeBranch (appendRunBranch r1 br2)
  NodeValueBranch v2 br2 -> NodeValueBranch v2 (appendRunBranch r1 br2)

appendRun :: Semigroup a => Node 'ChildRun a -> Run a -> Node 'ChildRun a
appendRun !x !r2 = case x of
  NodeValueNil v1 -> NodeValueRun v1 r2
  NodeValueRun v1 r1 -> case appendRuns r1 r2 of
    Left br -> NodeValueBranch v1 br
    Right r -> NodeValueRun v1 r
  NodeBranch br1 -> NodeBranch (appendBranchRun br1 r2)
  NodeValueBranch v1 br1 -> NodeValueBranch v1 (appendBranchRun br1 r2)

-- appendBranchRun :: Semigroup a
--   => Array (Node 'ChildBranch a)
--   -> Run a
--   -> Array (Node 'ChildBranch a)
-- appendBranchRun br (Run arr v) = if sizeofByteArray arr > 1
--   then _
--   else appendBranches br (singletonBranch (indexByteArray arr 0 :: Word8) v)

word8ToInt :: Word8 -> Int
word8ToInt = fromIntegral

-- precondition: the int must be between 0 and 255
intToWord8 :: Int -> Word8
intToWord8 (I# i) = W8# (int2Word# i)

-- precondition: size is greater than 1
tailByteArray :: ByteArray -> ByteArray
tailByteArray !arr = if debugMode && sizeofByteArray arr < 2
  then die "tailByteArray"
  else runST $ do
    let sz = sizeofByteArray arr - 1
    m <- newByteArray sz
    copyByteArray m 0 arr 1 sz
    unsafeFreezeByteArray m

-- Precondition: size is greater than 1. Yes, that is meant to
-- be 1 and not zero. This function is only ever used in
-- contexts where it is expected that the result has at least
-- one byte in it.
initByteArray :: ByteArray -> ByteArray
initByteArray !arr = if debugMode && sizeofByteArray arr < 2
  then die "initByteArray"
  else runST $ do
    let sz = sizeofByteArray arr - 1
    m <- newByteArray sz
    copyByteArray m 0 arr 0 sz
    unsafeFreezeByteArray m

dropByteArray :: Int -> ByteArray -> ByteArray
dropByteArray !n !arr = if debugMode && n > sizeofByteArray arr
  then die "dropByteArray"
  else runST $ do
    let sz = sizeofByteArray arr - n
    m <- newByteArray sz
    copyByteArray m 0 arr n sz
    unsafeFreezeByteArray m

takeByteArray :: Int -> ByteArray -> ByteArray
takeByteArray !n !arr = if debugMode && n > sizeofByteArray arr
  then die "takeByteArray"
  else runST $ do
    m <- newByteArray n
    copyByteArray m 0 arr 0 n
    unsafeFreezeByteArray m

die :: String -> a
die func = error ("Packed.Bytes.Trie." ++ func ++ ": invariant violated")

firstDifferentByte :: ByteArray -> ByteArray -> Int
firstDifferentByte a b = go 0
  where
  !n = min (sizeofByteArray a) (sizeofByteArray b)
  go !ix = if ix < n
    then if indexByteArray a ix == (indexByteArray b ix :: Word8)
      then go (ix + 1)
      else ix
    else ix
  

appendRuns :: Semigroup a
  => Run a
  -> Run a
  -> Either (ByteMap (Node 'ChildBranch a)) (Run a)
appendRuns (Run arr1 val1) (Run arr2 val2) =
  let !headByte1 = indexByteArray arr1 0 :: Word8
      !headByte2 = indexByteArray arr2 0 :: Word8
      !sz1 = sizeofByteArray arr1
      !sz2 = sizeofByteArray arr2
   in if headByte1 == headByte2
        then if sz1 > 1
          then if sz2 > 1
            then
              let !diffIx = firstDifferentByte arr1 arr2
                  !remSz1 = sizeofByteArray arr1 - diffIx
                  !remSz2 = sizeofByteArray arr2 - diffIx
               in case compare remSz1 1 of
                    LT -> if remSz2 < 1
                      then Right (Run arr1 (appendNode val1 val2)) 
                      else Right (Run arr1 (appendRun val1 (Run (dropByteArray diffIx arr2) val2)))
                    EQ -> case compare remSz2 1 of
                      LT -> Right (Run arr2 (prependRun (Run (dropByteArray diffIx arr1) val1) val2))
                      EQ -> Right $ Run
                        (initByteArray arr1)
                        (NodeBranch (BM.doubleton (indexByteArray arr1 diffIx) (coerceNode val1) (indexByteArray arr2 diffIx) (coerceNode val2)))
                      GT -> Right $ Run 
                        (initByteArray arr1)
                        (NodeBranch (BM.doubleton (indexByteArray arr1 diffIx) (coerceNode val1) (indexByteArray arr2 diffIx) (NodeRun (Run (dropByteArray (diffIx + 1) arr2) val2))))
                    GT -> case compare remSz2 1 of
                      LT -> Right (Run arr2 (prependRun (Run (dropByteArray diffIx arr1) val1) val2))
                      EQ -> Right $ Run
                        (initByteArray arr2)
                        (NodeBranch (BM.doubleton (indexByteArray arr1 diffIx) (NodeRun (Run (dropByteArray (diffIx + 1) arr1) val1)) (indexByteArray arr2 diffIx) (coerceNode val2)))
                      GT -> Right $ Run 
                        (takeByteArray diffIx arr1) -- this produces byte array of length > 0 since the head bytes must differ
                        (NodeBranch (BM.doubleton (indexByteArray arr1 diffIx) (NodeRun (Run (dropByteArray (diffIx + 1) arr1) val1)) (indexByteArray arr2 diffIx) (NodeRun (Run (dropByteArray (diffIx + 1) arr2) val2))))
            else Right (Run arr2 (prependRun (Run (tailByteArray arr1) val1) val2))
          else if sz2 > 1
            then Right (Run arr1 (appendRun val1 (Run (tailByteArray arr2) val2)))
            else Right (Run arr1 (appendNode val1 val2))
        else 
          let !n1 = if sizeofByteArray arr1 > 1
                then NodeRun (Run (tailByteArray arr1) val1)
                else coerceNode val1
              !n2 = if sizeofByteArray arr2 > 1
                then NodeRun (Run (tailByteArray arr2) val2)
                else coerceNode val2
           in Left (BM.doubleton headByte1 n1 headByte2 n2)

appendBranchRun :: Semigroup a
  => ByteMap (Node 'ChildBranch a)
  -> Run a
  -> ByteMap (Node 'ChildBranch a)
appendBranchRun !xs (Run arr val) =
  let headByte = indexByteArray arr 0 :: Word8
      node = fromMaybe NodeEmpty (BM.lookup headByte xs)
      -- When the size of the byte array is 1, there are no more
      -- bytes left in the run.
   in if sizeofByteArray arr > 1
        then BM.insert headByte (appendNode node (NodeRun (Run (tailByteArray arr) val))) xs
        else BM.insert headByte (appendNode node val) xs

-- runST $ do
--   m <- newArray 256 NodeEmpty
--   copyArray m 0 xs 0 256
--   let !headByte = indexByteArray arr 0 :: Word8
--       !headIx = word8ToInt headByte
--       !node = indexArray xs headIx
--   -- When the size of the byte array is 1, there are no more
--   -- bytes left in the run.
--   if sizeofByteArray arr > 1
--     then writeArray m headIx (appendNode node (NodeRun (Run (tailByteArray arr) val)))
--     else writeArray m headIx (appendNode node val)
--   unsafeFreezeArray m

appendRunBranch :: Semigroup a
  => Run a
  -> ByteMap (Node 'ChildBranch a)
  -> ByteMap (Node 'ChildBranch a)
appendRunBranch (Run arr val) !xs =
  let headByte = indexByteArray arr 0 :: Word8
      node = fromMaybe NodeEmpty (BM.lookup headByte xs)
      -- When the size of the byte array is 1, there are no more
      -- bytes left in the run.
   in if sizeofByteArray arr > 1
        then BM.insert headByte (appendNode (NodeRun (Run (tailByteArray arr) val)) node) xs
        else BM.insert headByte (appendNode val node) xs

appendBranches :: Semigroup a
  => ByteMap (Node 'ChildBranch a)
  -> ByteMap (Node 'ChildBranch a)
  -> ByteMap (Node 'ChildBranch a)
appendBranches = BM.appendWith appendNode

singleton :: Bytes -> a -> Trie a
singleton !b !a = if B.length b > 0
  then Trie (NodeRun (Run (B.toByteArray b) (NodeValueNil a)))
  else Trie (NodeValueNil a)

empty :: Trie a
empty = Trie NodeEmpty

ifoldMapArray :: Monoid m => (Int -> a -> m) -> Array a -> m
ifoldMapArray f arr = go 0 where
  go !ix = if ix < sizeofArray arr
    then mappend (f ix (indexArray arr ix)) (go (ix + 1))
    else mempty

fromList :: [(Bytes,a)] -> Trie a
fromList = fmap getFirst . fromListAppend . map (second First) 

fromListAppend :: Semigroup a => [(Bytes,a)] -> Trie a
fromListAppend = foldMap (uncurry singleton)

lookup :: Bytes -> Trie a -> Maybe a
lookup b0 (Trie node) = go b0 node where
  go :: Bytes -> Node c b -> Maybe b
  go !_ NodeEmpty = Nothing
  go !b1 (NodeValueNil v) = if B.null b1 then Just v else Nothing
  go !b1 (NodeBranch bm) = case B.uncons b1 of
    Just (!w,!b2) -> go b2 =<< BM.lookup w bm
    Nothing -> Nothing
  go !b1 (NodeValueBranch v bm) = case B.uncons b1 of
    Just (!w,!b2) -> go b2 =<< BM.lookup w bm
    Nothing -> Just v
  go !b1 (NodeRun (Run arr n)) = 
    let arrSz = sizeofByteArray arr in
    if B.length b1 < arrSz
      then Nothing
      else if B.unsafeTake arrSz b1 == B.fromByteArray arr
        then go (B.unsafeDrop arrSz b1) n
        else Nothing
  go !b1 (NodeValueRun v (Run arr n)) = 
    let arrSz = sizeofByteArray arr in
    if B.length b1 < 1
      then Just v
      else if B.length b1 < arrSz
        then Nothing
        else if B.unsafeTake arrSz b1 == B.fromByteArray arr
          then go (B.unsafeDrop arrSz b1) n
          else Nothing

toList :: Trie a -> [(Bytes,a)]
toList (Trie node) = go [] node
  where
  finalize = B.fromByteArray . BA.concatReversed
  go :: [ByteArray] -> Node c v -> [(Bytes,v)]
  go !_ NodeEmpty = []
  go !xs (NodeValueNil v) = [(finalize xs,v)]
  go !xs (NodeRun (Run arr n)) = go (arr : xs) n
  go !xs (NodeValueRun v (Run arr n)) = (finalize xs, v) : go (arr : xs) n
  go !xs (NodeBranch br) = BM.foldMapWithKey (\ix -> go (BA.singleton ix : xs)) br
  go !xs (NodeValueBranch v br) = (finalize xs, v) : BM.foldMapWithKey (\ix -> go (BA.singleton ix : xs)) br

instance Semigroup a => Semigroup (Trie a) where
  (<>) = append

instance Semigroup a => Monoid (Trie a) where
  mempty = empty

-- | Note: This does not have the key shadowing problem.
lookupM :: forall m a b. Monad m
  => m Word8 -- ^ get next byte
  -> (ByteArray -> m ()) -- ^ match many bytes 
  -> m Bool -- ^ test if we are finished
  -> (a -> m b) -- ^ convert trie value to result
  -> Trie a
  -> m (Either (Trie a) b)
{-# INLINE lookupM #-}
lookupM nextByte matchBytes testDone convert (Trie node) = go node where
  go :: Node f a -> m (Either (Trie a) b)
  go NodeEmpty = return (Left empty)
  go (NodeValueNil p) = do
    done <- testDone
    if done
      then fmap Right (convert p)
      else return (Left (Trie (NodeValueNil p)))
  go (NodeRun (Run arr n)) = matchBytes arr *> go n
  go (NodeBranch bm) = do
    b <- nextByte
    go (fromMaybe NodeEmpty (BM.lookup b bm))
  go (NodeValueRun p (Run arr n)) = do
    done <- testDone
    if done
      then fmap Right (convert p)
      else matchBytes arr *> go n
  go (NodeValueBranch p bm) = do
    done <- testDone
    if done
      then fmap Right (convert p)
      else do
        b <- nextByte
        go (fromMaybe NodeEmpty (BM.lookup b bm))

-- | Internal function for testing that invariants of the trie
-- are satisfied. 
valid :: Trie a -> Bool
valid (Trie node) = go node where
  go :: Node c b -> Bool
  go NodeEmpty = True
  go (NodeValueNil _) = True
  go (NodeRun (Run arr n)) = sizeofByteArray arr > 0 && go n
  go (NodeValueRun _ (Run arr n)) = sizeofByteArray arr > 0 && go n
  go (NodeBranch br) = SG.getAll (foldMap (SG.All . go) br)
  go (NodeValueBranch _ br) = SG.getAll (foldMap (SG.All . go) br)

