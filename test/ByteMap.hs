{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module ByteMap
  ( tests
  ) where

import Control.Monad (forM_)
import Data.ByteMap (ByteMap)
import Data.Monoid (Sum(..))
import Data.Proxy (Proxy(..))
import Data.Semigroup (First(..))
import Data.Word (Word8)
import Test.Tasty (TestTree,testGroup)
import Test.Tasty.HUnit (testCase,assertEqual,Assertion)
import qualified Data.ByteMap as BM
import qualified Test.Tasty.QuickCheck as TQC
import qualified Test.QuickCheck.Classes as QCC
import qualified Test.QuickCheck as QC

tests :: [TestTree]
tests =
  [ testGroup "insert"
    [ testCase "ascending" ascendingInsert
    , testCase "descending" descendingInsert
    , testGroup "zigzag"
      [ testCase "distant" zigzagInsert
      , testCase "extremes" zigzagExtremesInsert
      , testCase "adjacent" zigzapAdjacentInsert
      ]
    , testGroup "omnikeys"
      [ testCase "beginning" omnikeysBeginningInsert
      , testCase "middle" omnikeysMiddleInsert
      , testCase "ending" omnikeysEndingInsert
      ]
    ]
  , testGroup "lookup"
    [ testCase "above" aboveLookup
    , testCase "beneath" beneathLookup
    ]
  , testGroup "doubleton"
    [ testCase "low" lowDoubleton
    , testCase "high" highDoubleton
    ]
  , lawsToTest (QCC.isListLaws (Proxy :: Proxy (ByteMap Integer)))
  , lawsToTest (QCC.semigroupLaws (Proxy :: Proxy (ByteMap (First Integer))))
  , lawsToTest (QCC.commutativeMonoidLaws (Proxy :: Proxy (ByteMap (Sum Integer))))
  ]

lawsToTest :: QCC.Laws -> TestTree
lawsToTest (QCC.Laws name pairs) = testGroup name (map (uncurry TQC.testProperty) pairs)

instance QC.Arbitrary a => QC.Arbitrary (ByteMap a) where
  arbitrary = BM.fromList <$> QC.arbitrary

ascendingInsert :: Assertion
ascendingInsert = do
  let r = BM.toList $ BM.insert 250 'w' $ BM.insert 240 'x' $ BM.insert 110 'y' $ BM.insert 42 'z' BM.empty
  assertEqual "result" [(42,'z'),(110,'y'),(240,'x'),(250,'w')] r

descendingInsert :: Assertion
descendingInsert = do
  let r = BM.toList $ BM.insert 119 'w' $ BM.insert 121 'x' $ BM.insert 124 'y' $ BM.insert 129 'z' BM.empty
  assertEqual "result" [(119,'w'),(121,'x'),(124,'y'),(129,'z')] r

zigzagInsert :: Assertion
zigzagInsert = do
  let r = BM.toList $ BM.insert 237 'x' $ BM.insert 240 'y' $ BM.insert 235 'z' BM.empty
  assertEqual "result" [(235,'z'),(237,'x'),(240,'y')] r

zigzagExtremesInsert :: Assertion
zigzagExtremesInsert = do
  let r = BM.toList $ BM.insert 2 'x' $ BM.insert 0 'y' $ BM.insert 255 'z' BM.empty
  assertEqual "result" [(0,'y'),(2,'x'),(255,'z')] r

zigzapAdjacentInsert :: Assertion
zigzapAdjacentInsert = do
  let r = BM.toList $ BM.insert 238 'x' $ BM.insert 239 'y' $ BM.insert 237 'z' BM.empty
  assertEqual "result" [(237,'z'),(238,'x'),(239,'y')] r

omnikeysBeginningInsert :: Assertion
omnikeysBeginningInsert = do
  let bm = BM.fromList (take 255 (zip (enumFrom 1) (enumFrom 1))) :: ByteMap Integer
      r = BM.insert 0 (0 :: Integer) bm
  forM_ [0..255] $ \key -> do
    assertEqual "result" (Just (fromIntegral key)) (BM.lookup key r)

omnikeysEndingInsert :: Assertion
omnikeysEndingInsert = do
  let bm = BM.fromList (take 255 (zip (enumFrom 0) (enumFrom 0))) :: ByteMap Integer
      r = BM.insert 255 (255 :: Integer) bm
  forM_ [0..255] $ \key -> do
    assertEqual "result" (Just (fromIntegral key)) (BM.lookup key r)
  
omnikeysMiddleInsert :: Assertion
omnikeysMiddleInsert = do
  let bm = BM.fromList $
             (take 100 (zip (enumFrom 0) (enumFrom 0)))
             ++
             (take 155 (zip (enumFrom 101) (enumFrom 101)))
      r = BM.insert 100 (100 :: Integer) bm
  forM_ [0..255] $ \key -> do
    assertEqual "result" (Just (fromIntegral key)) (BM.lookup key r)

aboveLookup :: Assertion
aboveLookup = do
  let bm = BM.fromList [(12,'a'),(15,'b'),(242,'c')]
  assertEqual "result" Nothing (BM.lookup 243 bm)
  
beneathLookup :: Assertion
beneathLookup = do
  let bm = BM.fromList [(12,'a'),(15,'b'),(242,'c')]
  assertEqual "result" Nothing (BM.lookup 11 bm)

lowDoubleton :: Assertion
lowDoubleton = assertEqual "result"
  (BM.fromList [(1,'x'),(3,'y')])
  (BM.doubleton 1 'x' 3 'y')

highDoubleton :: Assertion
highDoubleton = assertEqual "result"
  (BM.fromList [(1,'x'),(3,'y')])
  (BM.doubleton 3 'y' 1 'x')
  
  
deriving instance QC.Arbitrary a => QC.Arbitrary (First a)

