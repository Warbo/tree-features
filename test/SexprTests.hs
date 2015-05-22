module SexprTests (tests, sanitise, exampleAsts) where

import           Test.Tasty (testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck
import Text.Parsec
import SexprHelper
import Features
import FeatureTest (sizedTreeOf)
import System.IO.Unsafe
import System.Directory

-- | Strip potentially unparsable characters from a string
sanitise :: String -> String
sanitise s = let keep c = c /= '"' && c /= '\\'
             in  filter keep (show s)

canParseChar c = sanitise [c] == [c] ==>
                 case parse stringChar "TEST" [c] of
                  Left err -> error (show err)
                  Right x  -> x == c

canParseQuote = case parse stringChar "TEST" ['\\', '"'] of
                 Left err -> error (show err)
                 Right x  -> x == 'z'

canParseString s' = let s = sanitise s'
                    in  case parse parseString "TEST" (quote s) of
                        Left err -> error (show err)
                        Right x  -> x == s

canParseLeaf s' = let s = sanitise s'
                  in  case parse parseLeaf "TEST" (quote s) of
                       Left err -> error (show err)
                       Right t  -> t == Leaf s

canParseFlatNode :: [String] -> Bool
canParseFlatNode ss' = let ss = map sanitise ss'
                       in case parse parseNode
                                     "TEST"
                                     ("(" ++ unwords (map quote ss) ++ ")") of
                           Left err -> error (show err)
                           Right t  -> t == Node (map Leaf ss)

-- NOTE: Parsed trees might not equal the incoming trees!
canParseRenderedTrees :: Int -> Property
canParseRenderedTrees n = forAll (sizedTreeOf n :: Gen (TreeOf String))
                                 parses
  where parses t' = let t = fmap sanitise t'
                    in  case parse parseExpr "TEST" (treeToSexpr t) of
                         Left  err -> error (show err)
                         Right _   -> True

parsedTreesEqualRendered :: Int -> Property
parsedTreesEqualRendered n = forAll (sizedTreeOf n :: Gen (TreeOf String))
                                    parses
  where parses t' = let t = fmap sanitise t'
                    in  case parse parseExpr "TEST" (treeToSexpr t) of
                         Left  err -> error (show err)
                         Right t'' -> t'' == t

exampleAsts = unsafePerformIO $ do files <- getDirectoryContents dir
                                   let asts = filter isAst files
                                   mapM (readFile . (dir ++)) asts
  where dir = "test/data/good/"
        isAst f = reverse (take 4 (reverse f)) == ".ast"

canParseHS2ASTOutput :: Bool
canParseHS2ASTOutput = all parser exampleAsts
  where parser ast = case parse parseExpr "TEST" ast of
                      Left err -> error (show err)
                      Right t  -> True

tests = testGroup "S-expression tests"
          [
            testProperty "canParseChar" canParseChar
          , testProperty "canParseQuote" canParseQuote
          , testProperty "canParseString" canParseString
          , testProperty "canParseLeaf" canParseLeaf
          , testProperty "canParseFlatNode" canParseFlatNode
          , testProperty "canParseRenderedTrees" canParseRenderedTrees
          , testProperty "canParseHS2ASTOutput" canParseHS2ASTOutput
          , testProperty "parsedTreesEqualRendered" parsedTreesEqualRendered
          ]
