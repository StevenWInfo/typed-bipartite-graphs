{-# LANGUAGE DataKinds #-}

module MapBipartiteTests
    ( tests
    ) where

import Test.Tasty
import Test.Tasty.HUnit

import MapBipartite as MB
import Bipartite

tests :: TestTree
tests = testGroup "MapBipartite Tests"
    [ testCase "Smoke Test" $
        genVertex markOne "foobar" @?= (Vertex "foobar" :: Vertex 1 String)
    , testCase "Basic inserts" $
        show ((insert $ ToRight fooOne barTwo) $ foldr insert emptyBipartite [ToLeft fooOne barTwo, ToLeft bazOne barTwo]) @?= "MapBipartite (fromList [(Vertex \"foo\",[Vertex \"bar\"])]) (fromList [(Vertex \"bar\",[Vertex \"foo\",Vertex \"baz\"])])"
    ]
    where fooOne = genVertex markOne "foo"
          barTwo = genVertex markTwo "bar"
          bazOne = genVertex markOne "baz"
