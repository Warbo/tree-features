import Features
import Text.XML.Light.Input
import Text.XML.Light.Types
import XmlHelper

bits = 31

main = fmap (parseRequest bits . parse) getContents >>= print
