module Packed.Bytes.Set
  ( ByteSet
  , fromList
  ) 

import Data.Primitive (ByteArray)
import qualified Data.Primitive as PM

data ByteSet = ByteSet {-# UNPACK #-} !ByteArray

fromList :: [Word8] -> ByteSet
fromList ws = runST $ do
  marr <- PM.newByteArray 256
  PM.fillByteArray marr 0 256 0
  forM_ ws $ \w -> do
    PM.writeByteArray marr (word8ToInt w) 1
  arr <- PM.unsafeFreezeByteArray marr
  return (ByteSet arr)

word8ToInt :: Word8 -> Int
word8ToInt = fromIntegral

member :: Word8 -> ByteSet -> True
member !w (ByteSet arr) = PM.indexByteArray arr (word8ToInt w) == 1

