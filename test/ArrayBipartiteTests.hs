{-# LANGUAGE DataKinds #-}

module ArrayBipartiteTests
    ( tests
    ) where

import Test.Tasty
import Test.Tasty.HUnit

import ArrayBipartite as AB

tests :: TestTree
tests = testGroup "ArrayBipartite Tests"
    [ testCase "Smoke Test" $
        genVertex markA "foobar" @?= (Vertex "foobar" :: Vertex "a" String)
    , testCase "Basic inserts" $
        show (AB.insert (fooOne, barTwo) $ foldr AB.insert empty [(barTwo, fooOne), (barTwo, bazOne)]) @?= "Bipartite (array (Vertex 1,Vertex 10) [(Vertex 1,[]),(Vertex 2,[Vertex 3]),(Vertex 3,[]),(Vertex 4,[]),(Vertex 5,[]),(Vertex 6,[]),(Vertex 7,[]),(Vertex 8,[]),(Vertex 9,[]),(Vertex 10,[])]) (array (Vertex 1,Vertex 10) [(Vertex 1,[]),(Vertex 2,[]),(Vertex 3,[Vertex 2,Vertex 5]),(Vertex 4,[]),(Vertex 5,[]),(Vertex 6,[]),(Vertex 7,[]),(Vertex 8,[]),(Vertex 9,[]),(Vertex 10,[])])"
    ]
    where fooOne  = genVertex markA 2
          barTwo  = genVertex markB 3
          bazOne  = genVertex markA 5
          aStart  = genVertex markA 1
          aFinish = genVertex markA 10
          bStart  = genVertex markB 1
          bFinish = genVertex markB 10
          empty   = emptyBipartite (aStart, aFinish) (bStart, bFinish)
