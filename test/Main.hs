module Main where

import Test.QuickCheck
import XmlTest
import ConvolutionTest
import qualified SexprTests
import qualified FeatureTest
import qualified ProgramTests

main = do putStrLn "convSameSize"
          quickCheck convSameSize
          putStrLn "parseTest"
          quickCheck parseTest
          putStrLn "parseTrees"
          quickCheck parseTrees
          FeatureTest.tests
          SexprTests.tests
          ProgramTests.tests
