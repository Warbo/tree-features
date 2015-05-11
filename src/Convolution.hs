module Convolution where

instance Num Bool where
  (+) = (/=)
  (*) = (&&)

  -- To prevent warnings
  abs         = id
  signum      = const True
  fromInteger = (/= 0)
  negate      = not

-- Get the bits of an Int and back again
i2b :: Integer -> [Bool]
i2b 0 = [False]
i2b 1 = [True]
i2b n = let (x, y) = n `divMod` 2
        in  i2b y ++ i2b x

b2i [True]  = 1
b2i [False] = 0
b2i (x:xs)  = b2i [x] + (2 * b2i xs)

-- Prepend leading zeros
pad n xs | length xs < n = False : pad (n-1) xs
pad n xs | otherwise     = xs

-- "Circular" list indexing
[] !!! _ = error "Cannot index an empty list"
xs !!! n = xs !! (n `mod` length xs)

-- Circular convolution of Ints, implemented bit-wise
cconv :: Integer -> Integer -> Integer
cconv x y = let x' = i2b x
                y' = i2b y
                n  = max (length x') (length y')
            in  b2i (cconv' (pad n x') (pad n y'))

-- Bitwise circular convolution
cconv' :: [Bool] -> [Bool] -> [Bool]
cconv' c x = let len = length c - 1
                 f   = cconvHelper c x
              in map f [0..len]

cconvHelper c x i = let len = length x - 1
                     in sum [(c !!! j) * (x !!! (i - j)) | j <- [0..len]]
