{-# OPTIONS_GHC -O2 -Wall #-}

import Gauge (bgroup,bench,whnf)
import Gauge.Main (defaultMain)
import Packed.Bytes.Small (ByteArray)
import Packed.Text (Text)
import Packed.Bytes (Bytes)
import Packed.Bytes.Table (BytesTable)
import Data.Primitive (Array)
import Data.HashMap.Strict (HashMap)
import Data.ByteString (ByteString)
import Data.Foldable (foldl',toList)
import Data.Maybe (isJust)
import Data.Char (ord,toUpper)
import GHC.Exts (fromList)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.HashMap.Strict as HM
import qualified Packed.Bytes as B
import qualified Data.Hashable as H
import qualified GHC.OldList as L
import qualified Packed.Bytes.Small as BA
import qualified Packed.Bytes.Table as BT
import qualified Packed.Text as T

main :: IO ()
main = do
  defaultMain
    [ bgroup "ByteArray"
      [ bench "findByte" $ whnf (BA.findByte 0x80) byteArrayA
      , bench "zipAnd" $ whnf (BA.zipAnd byteArrayA) byteArrayB
      , bgroup "hash"
        [ bgroup "packed"
            [ bench "large" $ whnf (BA.hash 0x80) byteArrayA
            , bench "small" $ whnf (BA.hash 0x80) byteArrayTiny
            ]
        , bgroup "platform"
            [ bench "large" $ whnf H.hash byteStringA
            , bench "small" $ whnf H.hash byteStringTiny
            ]
        ]
      , bgroup "Table"
        [ bench "packed" $ whnf packedLookupAll packedBytesTable
        , bench "platform" $ whnf platformLookupAll platformBytesTable
        ]
      ]
    , bgroup "Text"
      [ bgroup "toUpper"
        -- Not totally fair. The number 5000 refers to the character count,
        -- meaning that asian scripts end up with more bytes.
        [ bench "ascii" $ whnf T.toUpper textAscii5000
        , bench "latin" $ whnf T.toUpper textLatin5000
        , bench "asian" $ whnf T.toUpper textAsian5000
        ]
      , bgroup "decodeUtf8"
        [ bench "ascii" $ whnf T.decodeUtf8 encodedAscii5000
        , bench "latin" $ whnf T.decodeUtf8 encodedLatin5000
        , bench "asian" $ whnf T.decodeUtf8 encodedAsian5000
        ]
      ]
    ]

byteArrayA :: ByteArray
byteArrayA = BA.pack $ L.concat
  [ L.take 5000 (L.cycle (enumFromTo 0x00 0x7F))
  , [0x80]
  , L.take 2000 (L.cycle (enumFromTo 0xB0 0xFF))
  ]

byteStringA :: BS.ByteString
byteStringA = BS.pack (BA.unpack byteArrayA)

byteArrayB :: ByteArray
byteArrayB = BA.pack $ L.concat
  [ L.take 2000 (L.cycle (enumFromTo 0x80 0xFF))
  , [0x70]
  , L.take 3000 (L.cycle (enumFromTo 0x20 0x60))
  ]

byteArrayTiny :: ByteArray
byteArrayTiny = BA.take 20 byteArrayA

byteStringTiny :: BS.ByteString
byteStringTiny = BS.pack (BA.unpack byteArrayTiny)

encodedAscii5000 :: Bytes
encodedAscii5000 = T.encodeUtf8 textAscii5000

encodedLatin5000 :: Bytes
encodedLatin5000 = T.encodeUtf8 textLatin5000

encodedAsian5000 :: Bytes
encodedAsian5000 = T.encodeUtf8 textAsian5000

textAscii5000 :: Text
textAscii5000 = T.pack $ L.concat
  [ L.take 2000 (L.cycle (enumFromTo 'a' 'm' ++ [','] ++ enumFromTo 'A' 'H' ++ ['.']))
  , L.take 3000 (L.cycle "The Old Tavern Across The Town")
  ]

textLatin5000 :: Text
textLatin5000 = T.pack $ L.take 5000 $ L.cycle $ L.concat
  [ "Suzanne et Joseph étaient nés dans les deux premières années de leur. "
  , "arrivée à la colonie. Après la naissance de Suzanne, la mère abandonna "
  , "l'enseignement d'état. "
  , "Mentiría si dijera que era del todo nuevo el sentimiento de que ya no "
  , "iba a poder ser más que lo que era, que era un hombre que había "
  , "envejecido más de lo que suponía, que había sospechado tener toda "
  , "la vida por delante y había ido dejando pasar los años a la espera "
  , "de que llegara su momento, y ahora la tenía a su espalda. "
  ]

textAsian5000 :: Text
textAsian5000 = T.pack $ L.take 5000 $ L.cycle $ L.concat
  [ "유구한 역사와 전통에 빛나는 우리 대한 국민은 3·1 운동으로 건립된 "
  , "대한민국 임시 정부의 법통과 불의에 항거한 4·19 민주 이념을 계승하고, "
  , "조국의 민주 개혁과 평화적 통일의 사명에 입각하여 정의·인도와 동포애로써 "
  , "민족의 단결을 공고히 하고, 모든 사회적 폐습과 불의를 타파하며, 자율과 "
  , "조화를 바탕으로 자유 민주적 기본 질서를 더욱 확고히 하여 정치·경제·사회·문화의 "
  , "모든 영역에 있어서 각인의 기회를 균등히 하고, 능력을 최고도로 발휘하게 하며, "
  , "자유와 권리에 따르는 책임과 의무를 완수하게 하여, 안으로는 국민 생활의 균등한 "
  , "향상을 기하고 밖으로는 항구적인 세계 평화와 인류 공영에 이바지함으로써 우리들과 "
  , "우리들의 자손의 안전과 자유와 행복을 영원히 확보할 것을 다짐하면서 1948년 7월 12일에 "
  , "제정되고 8차에 걸쳐 개정된 헌법을 이제 국회의 의결을 거쳐 국민 투표에 의하여 개정한다."
  ]

-- textFrench :: Text
-- textFrench = T.pack $ L.concat
--   [ "Suzanne et Joseph étaient nés dans les deux premières années de leur. "
--   , "arrivée à la colonie. Après la naissance de Suzanne, la mère abandonna "
--   , "l'enseignement d'état. "
--   ]

platformBytesTable :: HashMap ByteString ()
platformBytesTable = HM.fromList
  $ map (\s -> (BC.pack s, ())) longEnglishWords

platformLongEnglishWords :: Array ByteString
platformLongEnglishWords = fromList
  $ map (\s -> BC.pack s) longEnglishWords

packedBytesTable :: BytesTable ()
packedBytesTable = BT.fromList
  $ fmap (\b -> (b,())) packedLongEnglishWordsList

packedLongEnglishWords :: Array Bytes
packedLongEnglishWords = fromList packedLongEnglishWordsList

packedLongEnglishWordsList :: [Bytes]
packedLongEnglishWordsList =
  fmap (\s -> B.pack (map (\c -> fromIntegral (ord c)) s)) longEnglishWords

packedLookupAll :: BytesTable () -> Bool
packedLookupAll t =
  foldl' (\acc b -> acc && isJust (BT.lookup b t)) True packedLongEnglishWords

platformLookupAll :: HashMap ByteString () -> Bool
platformLookupAll t =
  foldl' (\acc b -> acc && isJust (HM.lookup b t)) True platformLongEnglishWords

longEnglishWords :: [String]
longEnglishWords = map (map toUpper) xs ++ xs
  where 
  xs = 
    [ "sedimentologically"
    , "semiconservatively"
    , "semidomestications"
    , "semipermeabilities"
    , "semiprofessionally"
    , "semiquantitatively"
    , "sentimentalization"
    , "shortsightednesses"
    , "simplemindednesses"
    , "simultaneousnesses"
    , "sociopsychological"
    , "somnambulistically"
    , "soporiferousnesses"
    , "spectrofluorimeter"
    , "spectrofluorometer"
    , "spectrofluorometry"
    , "spectrographically"
    , "spectroheliographs"
    , "spectroheliography"
    , "spectrohelioscopes"
    , "spectrophotometers"
    , "spectrophotometric"
    , "sphygmomanometries"
    , "stereophotographic"
    , "stereoregularities"
    , "stereospecifically"
    , "stoichiometrically"
    , "stoutheartednesses"
    , "structuralizations"
    , "subcategorizations"
    , "subclassifications"
    , "subconsciousnesses"
    , "submicroscopically"
    , "substitutabilities"
    , "superadministrator"
    , "superciliousnesses"
    , "supercivilizations"
    , "superconglomerates"
    , "superintellectuals"
    , "superintelligences"
    , "factorization"
    , "factualnesses"
    , "facultatively"
    , "faddishnesses"
    , "faithlessness"
    , "fallibilities"
    , "falsification"
    , "familiarising"
    , "familiarities"
    , "familiarizing"
    , "fanaticalness"
    , "fantastically"
    , "fantasticated"
    , "fantasticates"
    , "farcicalities"
    , "farkleberries"
    , "fasciculation"
    , "fascinatingly"
    , "fascistically"
    , "fashionmonger"
    , "fatefulnesses"
    , "fatheadedness"
    , "fatuousnesses"
    , "faultfindings"
    , "faultlessness"
    , "faunistically"
    , "favorableness"
    , "fearfulnesses"
    , "feasibilities"
    , "featherbedded"
    , "featherbrains"
    , "featheredging"
    , "featherheaded"
    , "featherstitch"
    , "featherweight"
    , "feelingnesses"
    , "felicitations"
    , "fellmongeries"
    , "fellmongering"
    , "fellowshiping"
    , "fellowshipped"
    , "feloniousness"
    , "feminizations"
    , "fencelessness"
    , "fenestrations"
    , "fermentations"
    , "ferociousness"
    , "ferricyanides"
    , "ferrimagnetic"
    , "ferroconcrete"
    , "ferrocyanides"
    , "ferroelectric"
    , "ferromagnetic"
    , "ferrosilicons"
    , "fertilenesses"
    , "fertilization"
    , "festivalgoers"
    , "festivenesses"
    , "feudalization"
    , "feuilletonism"
    , "feuilletonist"
    , "fianchettoing"
    , "fiberglassing"
    , "fiberizations"
    , "fibrillations"
    , "fibrinolysins"
    , "fibrosarcomas"
    , "fibrovascular"
    , "fictionalised"
    , "fictionalises"
    , "fictionalized"
    , "fictionalizes"
    , "fictioneering"
    , "fictivenesses"
    , "fidgetinesses"
    , "fieldstripped"
    , "filibusterers"
    , "filibustering"
    , "filmographies"
    , "filterability"
    , "finalizations"
    , "fingerpicking"
    , "fingerprinted"
    , "finicalnesses"
    , "finickinesses"
    , "firefightings"
    , "fishabilities"
    , "fittingnesses"
    , "flabbergasted"
    , "flagellantism"
    , "flagellations"
    , "flamboyancies"
    , "flameproofers"
    , "flameproofing"
    , "flamethrowers"
    , "flavoproteins"
    , "flexibilities"
    , "flexographies"
    , "flightinesses"
    , "flirtatiously"
    , "flocculations"
    , "floodlighting"
    , "floricultural"
    , "floricultures"
    , "floristically"
    , "flourishingly"
    , "flowchartings"
    , "flowerinesses"
    , "fluctuational"
    , "flugelhornist"
    , "fluidextracts"
    , "fluidizations"
    , "fluorescences"
    , "fluoridations"
    , "fluorimetries"
    , "fluorinations"
    , "fluorocarbons"
    , "fluorochromes"
    , "fluorographic"
    , "fluorometries"
    , "fluoroscopies"
    , "fluoroscoping"
    , "fluoroscopist"
    , "fluorouracils"
    , "fluphenazines"
    , "flutterboards"
    , "focalizations"
    , "folkishnesses"
    , "followerships"
    , "foolhardiness"
    , "foolishnesses"
    , "foppishnesses"
    ]
