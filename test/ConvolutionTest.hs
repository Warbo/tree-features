module ConvolutionTest (tests) where

import           Test.Tasty (testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck
import Convolution

convSameSize x y = let len = max (length x) (length y)
                    in length (cconv' (pad len x) (pad len y)) == len

tests = testGroup "Convolution tests"
          [
            testProperty "Convolution preserves length" convSameSize
          ]
