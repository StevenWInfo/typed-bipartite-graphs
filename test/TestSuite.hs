{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import qualified MapBipartiteTests
import qualified ArrayBipartiteTests

main :: IO ()
main = defaultMain $ testGroup "Typed bipartite tests" 
    [ MapBipartiteTests.tests
    , ArrayBipartiteTests.tests
    ]
