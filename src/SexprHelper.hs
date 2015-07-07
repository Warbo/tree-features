module SexprHelper where

import qualified Data.AttoLisp              as L
import qualified Data.Attoparsec.ByteString as AB
import qualified Data.ByteString.Char8      as C
import qualified Data.Stringable            as S
import Features
import Text.ParserCombinators.Parsec

parseLisp = AB.maybeResult . AB.parse L.lisp

parseSexpr :: String -> TreeOf String
parseSexpr s = case parseLisp (S.fromString s) of
                Nothing -> error ("Failed to parse: " ++ s)
                Just x  -> lispToTree x

lispToTree (L.List   xs) = Node (map lispToTree xs)
lispToTree (L.String s)  = Leaf (S.toString s)

treeToLisp (Leaf x)  = L.String (S.fromString x)
treeToLisp (Node xs) = L.List (map treeToLisp xs)

treeToSexpr :: TreeOf String -> String
treeToSexpr = S.toString . L.encode . treeToLisp
