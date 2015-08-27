module Main where

import           Test.Tasty (defaultMain, testGroup)
import qualified XmlTest         as X
import qualified SexprTests      as S
import qualified FeatureTest     as F
import qualified ProgramTests    as P

main = defaultMain $ testGroup "All tests"
         [
           X.tests
         , S.tests
         , F.tests
         , P.tests
         ]
