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

data Mode = Sexpr | Xml

chooseMode :: IO Mode
chooseMode = do mode <- lookupEnv "MODE"
                return $ case mode of
                          Just "sexpr" -> Sexpr
                          _            -> Xml

getBits :: IO Int
getBits = do bits <- lookupEnv "BITS"
             return $ case bits of
                           Nothing -> defaultBits
                           Just n  -> read n

defaultBits :: (Integral a) => a
defaultBits = 31

parse :: Mode -> String -> TreeOf String
parse Xml   = (\el -> undefined) . parseXml
parse Sexpr = parseSexpr


mainTemplate' bits input mode = extractFeatures' . fmap (feature (fromIntegral bits))
                                                 . parse mode
                                                 $ input

mainTemplate bits input mode = numsOf bits (mainTemplate' bits input mode)

numsOf nums fv = let padded = padTo (fromIntegral nums) fv
                 in  addCommas (map show padded)

addCommas :: [String] -> String
addCommas s = intercalate "," s
--addCommas []       = []
--addCommas [x]      = [x]
--addCommas (x:y:zs) = x:',':addCommas (y:zs)

padTo :: Int -> FeatureVector -> FeatureVector
padTo n fv | length fv >= n = reverse (take n (reverse fv))
padTo n fv                  = padTo n (fv ++ [0])

mainWithBits = do bits  <- getBits
                  input <- getContents
                  mode  <- chooseMode
                  putStrLn (mainTemplate bits input mode)
