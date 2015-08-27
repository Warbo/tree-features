module Program where

import Data.List
import System.Environment
import Text.XML.Light.Input
import Text.XML.Light.Types
import Features
import SexprHelper
import XmlHelper
import Numeric (showHex, showIntAtBase)
import Data.Char (intToDigit)

-- Helpers for Main.hs, kept separate to ease testing

getBits :: IO Int
getBits = do bits <- lookupEnv "BITS"
             return $ case bits of
                           Nothing -> defaultBits
                           Just n  -> read n

defaultBits :: (Integral a) => a
defaultBits = 31

parse :: String -> TreeOf String
parse = parseSexpr

mainTemplate' bits input = extractFeatures' . fmap (feature (fromIntegral bits))
                                            . parse
                                            $ input

mainTemplate bits input = numsOf bits (mainTemplate' bits input)

numsOf nums fv = let padded = padTo (fromIntegral nums) fv
                 in  addCommas (map show padded)

addCommas :: [String] -> String
addCommas = intercalate ","

padTo :: Int -> FeatureVector -> FeatureVector
padTo n fv | length fv >= n = reverse (take n (reverse fv))
padTo n fv                  = padTo n (fv ++ [0])

mainWithBits = do bits  <- getBits
                  input <- getContents
                  putStrLn (mainTemplate bits input)
