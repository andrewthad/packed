module Packed.Bytes.Builder.IO
  ( fold
  ) where

import Data.Primitive (ByteArray)
import Packed.Bytes.Builder (Builder(..))

fold :: (ByteArray -> IO a) -> Builder -> IO ()
fold g (Builder f) = error "ueohtn"
  

