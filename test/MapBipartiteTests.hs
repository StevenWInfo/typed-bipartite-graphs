{-# LANGUAGE DataKinds #-}

module MapBipartiteTests
    ( tests
    ) where

import Test.Tasty
import Test.Tasty.HUnit

import MapBipartite

tests :: TestTree
tests = testGroup "MapBipartite Tests"
    [ testCase "Trying out tests" $
        genVertex markOne "foobar" @?= (Vertex "foobar" :: Vertex 1 String)
    ]
