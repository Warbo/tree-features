module ProgramTests where

import           Test.Tasty (testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Program
import Features
import SexprHelper
import Data.String.Utils
import qualified XmlTest
import qualified SexprTests
import qualified FeatureTest
import Test.QuickCheck

canParseSexpr = all ((> 0) . leaves) (map parse SexprTests.exampleAsts)

templateCanParseSexpr :: Int -> Int -> Property
templateCanParseSexpr b n = forAll (FeatureTest.sizedTreeOf n :: Gen (TreeOf String)) parses
  where bits = abs b `mod` 31 + 1
        parses t = inRange (mainTemplate' bits (treeToSexpr (fmap SexprTests.sanitise t)))
        inRange x = length x <= fromIntegral bits

templateGivesCsv :: Int -> Int -> Property
templateGivesCsv b n = forAll (FeatureTest.sizedTreeOf n :: Gen (TreeOf String)) parses
  where bits = abs b `mod` 31 + 1
        parses t = isCsv (mainTemplate bits (treeToSexpr (fmap SexprTests.sanitise t)))
        isCsv s = let ns = split "," s
                      isNum n = show (read n :: Int) == n
                      pos   n = (read n :: Int) >= 0
                  in  all isNum ns && all pos ns

tests = testGroup "Program tests"
          [
            testProperty  "canParseSexpr" canParseSexpr
          , testProperty  "templateCanParseSexpr" templateCanParseSexpr
          , testProperty  "templateGivesCsv" templateGivesCsv
          ]
