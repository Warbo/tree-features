module FeatureTest (tests, sizedTreeOf) where

import           Test.Tasty (testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Data.List
import Features
import Test.QuickCheck

-- Sized Trees have a decreasing parameter which bounds their size

sizedTreeOf :: Arbitrary a => Int -> Gen (TreeOf a)
sizedTreeOf n' = let n = abs n' `mod` 1000
                 in do head  <- arbitrary
                       count <- choose (1, n)
                       nums  <- numsSumTo n count
                       tail  <- mapM sizedTreeOf nums
                       return $ if n > 1
                                   then Node tail
                                   else Leaf head

sizedTree :: Int -> Gen Tree
sizedTree = fmap posTree . sizedTreeOf

sizedRequest :: Int -> Gen Request
sizedRequest n = do first  <- sizedTree n
                    second <- listOf (sizedTree n)
                    return (first, second)

-- Helpers for implementing sized Trees

posTree :: Num n => TreeOf [n] -> TreeOf [n]
posTree = fmap (map abs)

uniqueNumsTo size count = do nums <- numsTo size
                             return (takeUnique count nums [])

numsTo 0 = error "numsTo 0"
numsTo n = do head <- choose (1, n)
              tail <- numsTo n
              return (head : tail)

numsSumTo size n = do nums <- uniqueNumsTo size n
                      let diffed = diffs 0 (sort nums)
                      return $ filter (/= 0) (size - sum diffed : diffed)

diffs n []     = []
diffs n (x:xs) = x - n : diffs x xs

takeUnique 0 xs acc = acc
takeUnique n xs acc = let xs' = dropWhile (`elem` acc) xs
                      in  takeUnique (n-1) xs' (head xs' : acc)

-- Properties for Trees

extractFromTree n = forAll (sizedTree (1 + abs n)) positive
  where positive t = all (>= 0) (extractFeatures' t)

extractFromRequest n' = forAll (sizedRequest n) positive
  where n          = abs n' `mod` 1000
        positive r = all (>= 0) (extractFeatures r)

-- Properties for sized helpers

numsSumToSumTo :: Int -> Int -> Property
numsSumToSumTo size' n' = forAll (numsSumTo size n) sums
  where size    = (abs size' `mod` 1000) + n
        n       =  abs n'    `mod` 1000
        sums xs = sum xs == size

enoughUniqueNums :: Int -> Int -> Property
enoughUniqueNums size' count' = forAll (uniqueNumsTo size count) enough
  where enough = (== count) . length
        size   = abs size'  `mod` 1000
        count  = abs count' `mod` 1000

sizedTreeSized size' = forAll (sizedTree size) sized
  where size             = 1 + (abs size' `mod` 10)
        sized  t         = leaves t == size

tests = testGroup "Feature tests"
          [
            testProperty "numsSumToSumTo" numsSumToSumTo
          , testProperty "enoughUniqueNums" enoughUniqueNums
          , testProperty "sizedTreeSized" sizedTreeSized
          , testProperty "extractFromTree" extractFromTree
          , testProperty "extractFromRequest" extractFromRequest
          ]
