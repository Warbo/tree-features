module SexprHelper where

import Features
import Text.ParserCombinators.Parsec

-- We *could* use an existing sexpression parser, but it's really the simplest
-- thing in the world to do

stringChar :: Parser Char
stringChar = {-try (string "\\\"" >> return '"') <|> -} noneOf "\""

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
               return (Leaf s)

parseNode :: Parser (TreeOf String)
parseNode = do ts <- parseList
               return (Node ts)

parseExpr :: Parser (TreeOf String)
parseExpr = parseLeaf <|> parseNode

quote s = "\"" ++ s ++ "\""

treeToSexpr :: Show a => TreeOf a -> String
treeToSexpr (Leaf x)  = show x
treeToSexpr (Node ts) = let elems = map treeToSexpr ts
                        in  "(" ++ unwords elems ++ ")"

parseSexpr :: String -> TreeOf String
parseSexpr s = case parse parseExpr "parseExpr" s of
                Left  err -> error (show err)
                Right t   -> t
