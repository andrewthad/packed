{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_GHC -O2 #-}

module Packed.Bytes.Table
  ( BytesTable
  , construct
  , fromList
  ) where

import Data.Primitive (MutableByteArray)
import Packed.Bytes (Bytes)
import Control.Monad.ST (ST,runST)

data BytesTable v

data ByteArrayArray = ByteArrayArray ArrayArray#

data TableBuilder s v = TableBuilder
  {-# UNPACK #-} !Int -- total number of distinct ByteArrays
  {-# UNPACK #-} !(MutableArray s (ArrList v))

data ArrList v
  = ArrListCons !ByteArray !v !(ArrList v)
  | ArrListNil

-- This calls freeze on the arrays inside of the builder,
-- so do not reuse it after calling this function.
freezeBuilder :: TableBuilder s v -> ST s (BytesTable v)
freezeBuilder = _

construct :: forall v c.
  (forall s d. (Bytes -> v -> c -> d -> ST s d) -> c -> d -> ST s d)
  -> c 
  -> BytesTable v
construct f c0 = runST $ do
  tb <- microConstruct f c0 emptyTableBuilder
  freezeBuilder tb

microConstruct :: forall v c s.
     ((Bytes -> v -> c -> TableBuilder s v -> ST s (TableBuilder s v)) -> c -> TableBuilder s v -> ST s (TableBuilder s v))
  -> c -> TableBuilder s v -> ST s (TableBuilder s v)
microConstruct f c0 tb0 = f (\b v c d -> do
    d' <- insertBuilder d b v
    microConstruct f c d'
  ) c0 tb0

insertBuilder :: TableBuilder s v -> Bytes -> v -> ST s (TableBuilder s v)
insertBuilder (TableBuilder count marr0) key val = do
  marr1 <- if count < PM.sizeofMutableArray marr0
    then return marr0
    else 

rehash :: MutableArray s (ArrList v) -> MutableArray s (ArrList v)
rehash marr = do
  marrBig <- newArray (PM.sizeofMutableArray marr) ArrListNil
  copyMutableArray marrBig 0 



fromList :: [(Bytes,v)] -> BytesTable v
fromList = construct (\f xs d -> case xs of
    [] -> return d
    (b,v) : ys -> f b v ys d
  )

-- lookup :: Bytes -> BytesTable -> 

