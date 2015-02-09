module FeatureTest where

import Features
import Test.QuickCheck

instance Arbitrary Tree where
  arbitrary = do i  <- arbitrary
                 n  <- oneof $ map return treeList
                 ts <- listOf arbitrary
                 return (T (abs i, take n ts))

avg xs = fromIntegral (sum xs) / fromIntegral (length xs)

treeList = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 3, 4, 5]

-- == 1.0 allows infinite chains, > 1.0 explodes
finiteTrees = avg treeList < 1.0

extractFromTree t = extractFeatures' t >= 0

extractFromRequest r = extractFeatures r >= 0
