module Features where

import Convolution
import Data.Char
import Data.Hash.MD5
import Data.Maybe
import System.IO
import Text.XML.Light.Input
import Text.XML.Light.Types
import XmlHelper

data TreeOf a = Leaf a
              | Node [TreeOf a] deriving (Show, Eq)

instance Functor TreeOf where
  fmap f (Leaf x)  = Leaf (f x)
  fmap f (Node ts) = Node (map (fmap f) ts)

type Tree = TreeOf FeatureVector

-- | Goal and context, from Coq XML output
type Request = (Tree, [Tree])

type FeatureVector = [Integer]

parseRequest :: Integer -> Element -> Request
parseRequest bits e = let (goal:context) = elContent e
                       in (parseTerm bits goal, map (parseTerm bits) context)

parseTerm :: Integer -> Content -> Tree
parseTerm bits (Text t) = Leaf (feature bits (cdData t))
parseTerm bits (Elem e) = let subtrees = map (parseTerm bits) (elContent e)
                              name     = qName (elName e)
                              attrs    = getAttrs e
                              fVec     = features . map (feature bits) $ name : attrs
                          in Node (Leaf fVec : subtrees)

-- | Extract a FeatureVector from a Request
extractFeatures :: Request -> FeatureVector
extractFeatures (goal, context) = features (map extractFeatures' (goal : context))

-- | Extract a FeatureVector from a Tree
extractFeatures' :: Tree -> FeatureVector
extractFeatures' (Leaf x)  = features [x]
extractFeatures' (Node xs) = features (map extractFeatures' xs)

-- | Combine FeatureVectors
features :: [FeatureVector] -> FeatureVector
features []       = []
features (x:xs)   = zipSum x (features xs)

zipSum    []     ys  = ys
zipSum    xs     []  = xs
zipSum (x:xs) (y:ys) = (x+y) : zipSum xs ys

-- | Extract a feature from a string, mod n
feature :: Integer -> String -> FeatureVector
feature n = setFeature . (`mod` n) . md5i . Str

-- | Create a fresh FeatureVector with the nth feature set to 1 and the rest 0
setFeature :: Integer -> FeatureVector
setFeature 0 = [1]
setFeature n = 0 : setFeature (n-1)

leaves (Leaf _)  = 1
leaves (Node ts) = sum (map leaves ts)
