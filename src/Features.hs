module Features where

import Convolution
import Data.Char
import Data.Hash.MD5
import Data.Maybe
import System.IO
import Text.XML.Light.Input
import Text.XML.Light.Types
import XmlHelper

newtype Tree = T (Integer, [Tree]) -- Feature vector and sub-trees
             deriving (Show, Eq)

type Request = (Tree, [Tree])  -- Goal and context

parseRequest :: Integer -> Element -> Request
parseRequest bits e = let (goal:context) = elContent e
                       in (parseTerm bits goal, map (parseTerm bits) context)

parseTerm :: Integer -> Content -> Tree
parseTerm bits (Text t) = T (feature bits (cdData t), [])
parseTerm bits (Elem e) = let subtrees = map (parseTerm bits) (elContent e)
                              name     = qName (elName e)
                              attrs    = getAttrs e
                              fVec     = features . map (feature bits) $ name : attrs
                          in T (fVec, subtrees)

extractFeatures :: Request -> Integer
extractFeatures (goal, context) = features (map extractFeatures' (goal : context))

extractFeatures' :: Tree -> Integer
extractFeatures' (T (x, xs)) = features (x : map extractFeatures' xs)

features :: [Integer] -> Integer
features []  = 0
features [x] = x
features (x:y:[]) = cconv x y
features (x:y:xs) = cconv (cconv x y) (features xs)  -- Left-associative

feature :: Integer -> String -> Integer
feature n = setBit . (`mod` n) . md5i . Str

setBit :: Integer -> Integer
setBit = (2 ^)
