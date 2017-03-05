{-# LANGUAGE DataKinds #-}

module ArrayBipartiteTests
    ( tests
    ) where

import Test.Tasty
import Test.Tasty.HUnit

import ArrayBipartite as AB
import Bipartite

tests :: TestTree
tests = testGroup "ArrayBipartite Tests"
    [ testCase "Smoke Test" $
        genVertex markOne "foobar" @?= (Vertex "foobar" :: Vertex 1 String)
    , testCase "Basic inserts" $
        show (insert (fooOne, barTwo) $ foldr insert empty [(barTwo, fooOne), (barTwo, bazOne)]) @?= "ArrayBipartite (array (Vertex 1,Vertex 10) [(Vertex 1,[]),(Vertex 2,[Vertex 3]),(Vertex 3,[]),(Vertex 4,[]),(Vertex 5,[]),(Vertex 6,[]),(Vertex 7,[]),(Vertex 8,[]),(Vertex 9,[]),(Vertex 10,[])]) (array (Vertex 1,Vertex 10) [(Vertex 1,[]),(Vertex 2,[]),(Vertex 3,[Vertex 2,Vertex 5]),(Vertex 4,[]),(Vertex 5,[]),(Vertex 6,[]),(Vertex 7,[]),(Vertex 8,[]),(Vertex 9,[]),(Vertex 10,[])])"
    ]
    where fooOne  = genVertex markOne 2
          barTwo  = genVertex markTwo 3
          bazOne  = genVertex markOne 5
          aStart  = genVertex markOne 1
          aFinish = genVertex markOne 10
          bStart  = genVertex markTwo 1
          bFinish = genVertex markTwo 10
          empty   = emptyBipartite (aStart, aFinish) (bStart, bFinish)
