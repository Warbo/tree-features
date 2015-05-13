module SexprHelper where

import Features
import Text.ParserCombinators.Parsec

-- We *could* use an existing sexpression parser, but it's really the simplest
-- thing in the world to do

stringChar :: Parser Char
stringChar = noneOf "\""

parseString :: Parser String
parseString = between (char '"')
                      (char '"')
                      (many stringChar)

parseList :: Parser [TreeOf String]
parseList = between (char '(')
                    (char ')')
                    (sepBy parseExpr spaces)

parseLeaf :: Parser (TreeOf String)
parseLeaf = do s <- parseString
               return (T (s, []))

parseNode :: Parser (TreeOf String)
parseNode = do ts <- parseList
               return (T ("", ts))

parseExpr :: Parser (TreeOf String)
parseExpr = parseLeaf <|> parseNode

quote s = "\"" ++ s ++ "\""

treeToSexpr :: Show a => TreeOf a -> String
treeToSexpr (T (x, [])) = show x
treeToSexpr (T (x, ts)) = let elems = show x : map treeToSexpr ts
                          in  "(" ++ unwords elems ++ ")"
