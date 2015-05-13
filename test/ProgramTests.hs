module ProgramTests where

import Program
import Features
import SexprHelper
import qualified XmlTest
import qualified SexprTests
import qualified FeatureTest
import Test.QuickCheck

canParseXml   = all ((> 0) . leaves) [parse Xml XmlTest.testData]

canParseSexpr = all ((> 0) . leaves) (map (parse Sexpr) SexprTests.exampleAsts)

templateCanParseSexpr b n = forAll (FeatureTest.sizedTreeOf n :: Gen (TreeOf String)) parses
  where bits = abs b `mod` 31 + 1
        parses t = inRange (mainTemplate bits (treeToSexpr (fmap SexprTests.sanitise t)) Sexpr)
        inRange x = x >= 0 && x < (2 ^ bits)

tests = do putStrLn  "canParseXml"
           quickCheck canParseXml
           putStrLn  "canParseSexpr"
           quickCheck canParseSexpr
           putStrLn  "templateCanParseSexpr"
           quickCheck templateCanParseSexpr
