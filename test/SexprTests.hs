module SexprTests where

import Test.QuickCheck
import Text.Parsec
import SexprHelper
import Features
import FeatureTest
import System.IO.Unsafe
import System.Directory

sanitise = filter (/= '"')

leaf s = T (s, [])

canParseChar c = sanitise [c] == [c] ==>
                 case parse stringChar "TEST" [c] of
                  Left err -> error (show err)
                  Right x  -> x == c

canParseString s' = let s = sanitise s'
                    in  case parse parseString "TEST" (quote s) of
                        Left err -> error (show err)
                        Right x  -> x == s

canParseLeaf s' = let s = sanitise s'
                  in  case parse parseLeaf "TEST" (quote s) of
                       Left err -> error (show err)
                       Right t  -> t == T (s, [])

canParseFlatNode :: [String] -> Bool
canParseFlatNode ss' = let ss = map sanitise ss'
                       in case parse parseNode
                                     "TEST"
                                     ("(" ++ unwords (map quote ss) ++ ")") of
                           Left err -> error (show err)
                           Right t  -> t == T ("", map leaf ss)

-- NOTE: Parsed trees might not equal the incoming trees!
canParseRenderedTrees :: Int -> Property
canParseRenderedTrees n = forAll (sizedTreeOf n :: Gen (TreeOf String))
                                 parses
  where parses t' = let t = fmap sanitise t'
                    in  case parse parseExpr "TEST" (treeToSexpr t) of
                         Left err -> error (show err)
                         Right _  -> True

exampleAsts = unsafePerformIO $ do files <- getDirectoryContents dir
                                   let asts = filter isAst files
                                   mapM (readFile . (dir ++)) asts
  where dir = "test/data/good/"
        isAst f = reverse (take 4 (reverse f)) == ".ast"

canParseHS2ASTOutput :: Int -> Bool
canParseHS2ASTOutput n = let ast = exampleAsts !! (n `mod` length exampleAsts)
                         in  case parse parseExpr "TEST" ast of
                              Left err -> error (show err)
                              Right t  -> error ("GOT: " ++ show t)

tests = do putStrLn  "canParseChar"
           quickCheck canParseChar
           putStrLn  "canParseString"
           quickCheck canParseString
           putStrLn  "canParseLeaf"
           quickCheck canParseLeaf
           putStrLn  "canParseFlatNode"
           quickCheck canParseFlatNode
           putStrLn  "canParseRenderedTrees"
           quickCheck canParseRenderedTrees
           putStrLn  "canParseHS2ASTOutput"
           quickCheck canParseHS2ASTOutput
