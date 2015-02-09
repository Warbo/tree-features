module ConvolutionTest where

import Test.QuickCheck
import Convolution

convSameSize x y = let len = max (length x) (length y)
                    in length (cconv' (pad len x) (pad len y)) == len
