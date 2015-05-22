module ProgramTests where

import           Test.Tasty (testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Program
import Features
import SexprHelper
import qualified XmlTest
import qualified SexprTests
import qualified FeatureTest
import Test.QuickCheck

canParseXml   = all ((> 0) . leaves) [parse Xml XmlTest.testData]

canParseSexpr = all ((> 0) . leaves) (map (parse Sexpr) SexprTests.exampleAsts)

templateCanParseSexpr :: Int -> Int -> Property
templateCanParseSexpr b n = forAll (FeatureTest.sizedTreeOf n :: Gen (TreeOf String)) parses
  where bits = abs b `mod` 31 + 1
        parses t = inRange (mainTemplate' bits (treeToSexpr (fmap SexprTests.sanitise t)) Sexpr)
        inRange x = x >= (0 :: Integer) && x < (2 ^ bits)

templateGivesCsv :: Int -> Int -> Property
templateGivesCsv b n = forAll (FeatureTest.sizedTreeOf n :: Gen (TreeOf String)) parses
  where bits = abs b `mod` 31 + 1
        parses t = isCsv (mainTemplate bits (treeToSexpr (fmap SexprTests.sanitise t)) Sexpr)
        isCsv [] = True
        isCsv [c] = True
        isCsv ('0':',':bs) = isCsv bs
        isCsv ('1':',':bs) = isCsv bs

tests = testGroup "Program tests"
          [
            testProperty  "canParseXml" canParseXml
          , testProperty  "canParseSexpr" canParseSexpr
          , testProperty  "templateCanParseSexpr" templateCanParseSexpr
          , testProperty  "templateGivesCsv" templateGivesCsv
          ]
