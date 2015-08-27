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

quote x = concat ["\"", x, "\""]

canParseLeaf s' = let s = sanitise s'
                      t = parseSexpr (quote s)
                   in t == Leaf s

canParseFlatNode :: [String] -> Bool
canParseFlatNode ss' = let ss = map sanitise ss'
                           t  = parseSexpr (concat ["(",
                                                    unwords (map quote ss),
                                                    ")"])
                        in t == Node (map Leaf ss)

-- NOTE: Parsed trees might not equal the incoming trees!
canParseRenderedTrees :: Int -> Property
canParseRenderedTrees n = forAll (sizedTreeOf n :: Gen (TreeOf String))
                                 parses
  where parses t' = let t  = fmap sanitise t'
                     in forceTree (parseSexpr (treeToSexpr t))

parsedTreesEqualRendered :: Int -> Property
parsedTreesEqualRendered n = forAll (sizedTreeOf n :: Gen (TreeOf String))
                                    parses
  where parses t' = let t  = fmap sanitise t'
                        t2 = parseSexpr (treeToSexpr t)
                     in t2 == t

{-# NOINLINE exampleAsts #-}
exampleAsts = unsafePerformIO $ do files <- getDirectoryContents dir
                                   let asts = filter isAst files
                                   mapM (readFile . (dir ++)) asts
  where dir = "test/data/good/"
        isAst f = reverse (take 4 (reverse f)) == ".ast"

forceTree :: TreeOf a -> Bool
forceTree (Leaf _)  = True
forceTree (Node xs) = all forceTree xs

canParseHS2ASTOutput :: Bool
canParseHS2ASTOutput = all parser exampleAsts
  where parser ast = forceTree (parseSexpr ast)

tests = testGroup "S-expression tests"
          [
            testProperty "canParseLeaf" canParseLeaf
          , testProperty "canParseFlatNode" canParseFlatNode
          , testProperty "canParseRenderedTrees" canParseRenderedTrees
          , testProperty "canParseHS2ASTOutput" canParseHS2ASTOutput
          , testProperty "parsedTreesEqualRendered" parsedTreesEqualRendered
          ]
