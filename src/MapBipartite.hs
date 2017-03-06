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

It occurs to me that having to make two instances of a multiparameter typeclass might create more work in other parts of the program. If you are handling vertices without knowing without having concrete types and you insert edges going in both directions of the bipartite graph, you'll have to put typeclass constraints for both instances. However, perhaps interactions with both sides of the graph aren't as common. Maybe they are though. Not sure.

I'm sort of flip flopping between being specific with two partitions in MapBipartite with the type literals 1 and 2, but then being more general with Bipartition. Should probably be consistant.
 -}

-- Has general types, but only useful for bipartites right now. Only a relationship between two partitions, rather than a partition and all other partitions for multipartite graphs.
type Bipartition m x n y = Map (Vertex m x) [Vertex n y]

data MapBipartite x y = MapBipartite (Bipartition 1 x 2 y) (Bipartition 2 y 1 x) deriving (Show)

emptyPartition :: Bipartition m x n y
emptyPartition = Map.empty

emptyBipartite :: MapBipartite x y
emptyBipartite = MapBipartite (emptyPartition :: Bipartition 1 x 2 y) (emptyPartition :: Bipartition 2 y 1 x)

insertPart :: (Ord x) => (Vertex m x, Vertex n y) -> Bipartition m x n y -> Bipartition m x n y
insertPart (vertM, vertN) part = Map.insertWith ifExists vertM [vertN] part
    where ifExists new old = new ++ old

instance Bipartite MapBipartite where
    -- empty = emptyBipartite
    insert (ToRight v1 v2) (MapBipartite partOne partTwo) = MapBipartite (insertPart (v1, v2) partOne) partTwo
    insert (ToLeft v1 v2) (MapBipartite partOne partTwo) = MapBipartite partOne (insertPart (v2, v1) partTwo)
