{-# OPTIONS_GHC -O2 -Wall #-}

import Gauge (bgroup,bench,whnf)
import Gauge.Main (defaultMain)
import Byte.Array (ByteArray)
import Text.Slice (Text)

import qualified Byte.Array as BA
import qualified Text.Slice as T
import qualified GHC.OldList as L

main :: IO ()
main = do
  defaultMain
    [ bgroup "ByteArray"
      [ bench "findByte" $ whnf (BA.findByte 0x80) byteArrayA
      , bench "zipAnd" $ whnf (BA.zipAnd byteArrayA) byteArrayB
      ]
    , bgroup "Text"
      [ bgroup "toUpper"
        [ bench "ascii" $ whnf T.toUpper textAscii
        , bench "latin" $ whnf T.toUpper textLatin
        ]
      ]
    ]

byteArrayA :: ByteArray
byteArrayA = BA.pack $ L.concat
  [ L.take 5000 (L.cycle (enumFromTo 0x00 0x7F))
  , [0x80]
  , L.take 2000 (L.cycle (enumFromTo 0xB0 0xFF))
  ]

byteArrayB :: ByteArray
byteArrayB = BA.pack $ L.concat
  [ L.take 2000 (L.cycle (enumFromTo 0x80 0xFF))
  , [0x70]
  , L.take 3000 (L.cycle (enumFromTo 0x20 0x60))
  ]

textAscii :: Text
textAscii = T.pack $ L.concat
  [ L.take 2000 (L.cycle (enumFromTo 'a' 'm' ++ [','] ++ enumFromTo 'A' 'H' ++ ['.']))
  , L.take 3000 (L.cycle "The Old Tavern Across The Town")
  ]

textLatin :: Text
textLatin = T.pack $ L.take 5000 $ L.cycle $ L.concat
  [ "Suzanne et Joseph étaient nés dans les deux premières années de leur. "
  , "arrivée à la colonie. Après la naissance de Suzanne, la mère abandonna "
  , "l'enseignement d'état. "
  , "Mentiría si dijera que era del todo nuevo el sentimiento de que ya no "
  , "iba a poder ser más que lo que era, que era un hombre que había "
  , "envejecido más de lo que suponía, que había sospechado tener toda "
  , "la vida por delante y había ido dejando pasar los años a la espera "
  , "de que llegara su momento, y ahora la tenía a su espalda. "
  ]
