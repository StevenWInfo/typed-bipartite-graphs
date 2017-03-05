{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module MapBipartite
    ( Vertex (..)
    , Bipartite (..)
    , Bipartition
    , Edge
    , genVertex
    , markOne
    , markTwo
    , emptyBipartite
    , InsertEdge (insert)
    ) where

import GHC.TypeLits
import Data.Map.Strict as Map
import Data.Proxy

{-
Ajacency list representation of a bipartite graph using maps.

This has the advantage of also not having to maintain the array range invariants. This simplifies things and also removes any kind of check to manipulate a bipartite graph.

I suppose you could make a function that wasn't polymorphic and which might be slightly more efficient if you knew which partition a vertex goes into. Not sure exactly how polymorphism affects performance though.
 -}

newtype Vertex (n :: Nat) a = Vertex a deriving (Eq, Ord, Show)

-- Applicative and Monad too?
instance Functor (Vertex n) where
    fmap f (Vertex a) = Vertex (f a)

type Bipartition m x n y = Map (Vertex m x) [Vertex n y]

data Bipartite x y = Bipartite (Bipartition 1 x 2 y) (Bipartition 2 y 1 x) deriving (Show)

type Edge m x n y = (Vertex m x, Vertex n y)

genVertex :: Proxy (n :: Nat) -> a -> Vertex n a
genVertex _ a = Vertex a

markOne :: Proxy 1
markOne = Proxy

markTwo :: Proxy 2
markTwo = Proxy

emptyPartition :: Bipartition m x n y
emptyPartition = Map.empty

emptyBipartite :: Bipartite x y
emptyBipartite = Bipartite (emptyPartition :: Bipartition 1 x 2 y) (emptyPartition :: Bipartition 2 y 1 x)

insertPart :: (Ord x) => Edge m x n y -> Bipartition m x n y -> Bipartition m x n y
insertPart (vertM, vertN) part = Map.insertWith ifExists vertM [vertN] part
    where ifExists new old = new ++ old

class InsertEdge edge bp | edge -> bp where
    insert :: edge -> bp -> bp

instance (Ord x) => InsertEdge (Edge 1 x 2 y) (Bipartite x y) where
    insert edge (Bipartite partOne partTwo) = Bipartite (insertPart edge partOne) partTwo

instance (Ord y) => InsertEdge (Edge 2 y 1 x) (Bipartite x y) where
    insert edge (Bipartite partOne partTwo) = Bipartite partOne (insertPart edge partTwo)
