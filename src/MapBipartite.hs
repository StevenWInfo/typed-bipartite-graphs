{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module MapBipartite
    ( MapBipartite (..)
    , Bipartition
    , genVertex
    , markOne
    , markTwo
    , emptyBipartite
    ) where

import GHC.TypeLits
import Data.Map.Strict as Map
import Bipartite as BP

{-
Ajacency list representation of a bipartite graph using maps.

This has the advantage of also not having to maintain the array range invariants. This simplifies things and also removes any kind of check to manipulate a bipartite graph.

I suppose you could make a function that wasn't polymorphic and which might be slightly more efficient if you knew which partition a vertex goes into. Not sure exactly how polymorphism affects performance though.
 -}

type Bipartition m x n y = Map (Vertex m x) [Vertex n y]

data MapBipartite x y = MapBipartite (Bipartition 1 x 2 y) (Bipartition 2 y 1 x) deriving (Show)

emptyPartition :: Bipartition m x n y
emptyPartition = Map.empty

emptyBipartite :: MapBipartite x y
emptyBipartite = MapBipartite (emptyPartition :: Bipartition 1 x 2 y) (emptyPartition :: Bipartition 2 y 1 x)

insertPart :: (Ord x) => Edge m x n y -> Bipartition m x n y -> Bipartition m x n y
insertPart (vertM, vertN) part = Map.insertWith ifExists vertM [vertN] part
    where ifExists new old = new ++ old

instance (Ord x) => Bipartite (Edge 1 x 2 y) (MapBipartite x y) where
    insert edge (MapBipartite partOne partTwo) = MapBipartite (insertPart edge partOne) partTwo

instance (Ord y) => Bipartite (Edge 2 y 1 x) (MapBipartite x y) where
    insert edge (MapBipartite partOne partTwo) = MapBipartite partOne (insertPart edge partTwo)
