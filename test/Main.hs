module Main where

import Test.QuickCheck
import XmlTest
import ConvolutionTest
import FeatureTest

main = do putStrLn "convSameSize"
          quickCheck convSameSize
          putStrLn "extractFromTree"
          quickCheck extractFromTree
          putStrLn "extractFromRequest"
          quickCheck extractFromRequest
          putStrLn "parseTest"
          quickCheck parseTest
          putStrLn "parseTrees"
          quickCheck parseTrees
