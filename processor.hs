import Features
import System.Environment
import Text.XML.Light.Input
import Text.XML.Light.Types
import XmlHelper

getBits :: (Integral a, Read a) => IO a
getBits = do args <- getArgs
             return $ case (map read args) of
                           []    -> defaultBits
                           (n:_) -> n

defaultBits :: (Integral a) => a
defaultBits = 31

main = do bits  <- getBits
          input <- getContents
          print . extractFeatures . parseRequest bits . parse $ input
