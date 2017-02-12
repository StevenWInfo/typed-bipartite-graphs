{-# LANGUAGE DataKinds #-}

module MapBipartiteTests
    ( tests
    ) where

import Test.Tasty
import Test.Tasty.HUnit

import MapBipartite as MB

tests :: TestTree
tests = testGroup "MapBipartite Tests"
    [ testCase "Trying out tests" $
        genVertex markOne "foobar" @?= (Vertex "foobar" :: Vertex 1 String)
    , testCase "Basic inserts" $
        show (MB.insert (fooOne, barTwo) $ foldr MB.insert emptyBipartite [(barTwo, fooOne), (barTwo, bazOne)]) @?= "Bipartite (fromList [(Vertex \"foo\",[Vertex \"bar\"])]) (fromList [(Vertex \"bar\",[Vertex \"foo\",Vertex \"baz\"])])"
    ]
    where fooOne = genVertex markOne "foo"
          barTwo = genVertex markTwo "bar"
          bazOne = genVertex markOne "baz"
