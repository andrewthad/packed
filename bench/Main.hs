{-# OPTIONS_GHC -O2 -Wall #-}

import Gauge (bgroup,bench,whnf)
import Gauge.Main (defaultMain)
import Byte.Array (ByteArray)

import qualified Byte.Array as BA
import qualified GHC.OldList as L

main :: IO ()
main = do
  defaultMain
    [ bgroup "ByteArray"
      [ bench "findByte" $ whnf (BA.findByte 0x80) byteArrayA
      ]
    ]

byteArrayA :: ByteArray
byteArrayA = BA.pack $ L.concat
  [ L.take 5000 (L.cycle (enumFromTo 0x00 0x7F))
  , [0x80]
  , L.take 2000 (L.cycle (enumFromTo 0xB0 0xFF))
  ]
