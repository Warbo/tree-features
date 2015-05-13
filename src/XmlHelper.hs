module XmlHelper where

import Data.Char
import Data.Maybe
import Text.XML.Light.Input
import Text.XML.Light.Types

name :: Element -> String
name = map toLower . qName . elName

countLeaves :: Element -> Int
countLeaves = undefined

getAttrs :: Element -> [String]
getAttrs = concatMap (attrFilter . attrStrs) . elAttribs

attrStrs :: Attr -> (String, String)
attrStrs a = (qName (attrKey a), attrVal a)

attrFilter :: (String, String) -> [String]
attrFilter ("id", _) = []
attrFilter (x,    y) = [x ++ y]

parseXml = fromJust . parseXMLDoc

getXml = fmap parseXml getContents
