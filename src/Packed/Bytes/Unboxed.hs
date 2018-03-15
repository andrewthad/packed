{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnboxedSums #-}

module Packed.Bytes.Unboxed
  ( Bytes#
  ) where

type Bytes# = (# ByteArray#, Int#, Int# #)

