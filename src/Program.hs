module Program where

import System.Environment
import Text.XML.Light.Input
import Text.XML.Light.Types
import Features
import SexprHelper
import XmlHelper

-- Helpers for Main.hs, kept separate to ease testing

data Mode = Sexpr | Xml

chooseMode :: IO Mode
chooseMode = do mode <- lookupEnv "MODE"
                return $ case mode of
                          Just "sexpr" -> Sexpr
                          _            -> Xml

getBits :: (Integral a, Read a) => IO a
getBits = do args <- getArgs
             return $ case (map read args) of
                           []    -> defaultBits
                           (n:_) -> n

defaultBits :: (Integral a) => a
defaultBits = 31

parse :: Mode -> String -> TreeOf String
parse Xml   = (\el -> undefined) . parseXml
parse Sexpr = parseSexpr

mainTemplate bits input mode = extractFeatures' . fmap (feature bits)
                                                . parse mode
                                                $ input

mainWithBits = do bits  <- getBits
                  input <- getContents
                  mode  <- chooseMode
                  print (mainTemplate bits input mode)
