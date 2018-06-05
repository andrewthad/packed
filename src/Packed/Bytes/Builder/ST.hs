module Packed.Bytes.Builder.ST
  ( fold
  ) where

import GHC.Exts (State#,MutableByteArray#,Int#)
import GHC.Exts (copyMutableByteArray#,newByteArray#,unsafeFreezeByteArray#,shrinkMutableByteArray#,getSizeofMutableByteArray#)
import GHC.Exts ((+#),(>#),(*#))

fold :: (ByteArray -> ST s a) -> Builder -> ST s ()
fold (IO g) (Builder f) = error "ueohtneu"

