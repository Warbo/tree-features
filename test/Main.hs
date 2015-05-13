module Main where

import Test.QuickCheck
import XmlTest
import ConvolutionTest
import qualified FeatureTest

main = do putStrLn "convSameSize"
          quickCheck convSameSize
          putStrLn "parseTest"
          quickCheck parseTest
          putStrLn "parseTrees"
          quickCheck parseTrees
          FeatureTest.tests
