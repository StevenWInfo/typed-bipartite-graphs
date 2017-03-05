{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module ArrayBipartite
    ( ArrayBipartite (..)
    , Bipartition
    -- , insertPart -- Don't export
    -- , emptyPartition -- Don't export
    , emptyBipartite
    ) where

import GHC.TypeLits
import Data.Array
import Data.Proxy
import Bipartite

-- I suppose if both partitions are part of the same graph then maybe they should have the same type of "held value". Also simpler.
-- Might be more useful if they are able to have different types though.
data ArrayBipartite x y = ArrayBipartite (Bipartition 1 x 2 y) (Bipartition 2 y 1 x) deriving (Show)

type Bipartition m x n y = Array (Vertex m x) [Vertex n y]

-- Don't export
insertPart :: (Ix x, Ix y) => Bipartition m x n y -> Edge m x n y -> Bipartition m x n y
insertPart part (vertM, vertN) = part // [(vertM, vertN : (part ! vertM))]

-- Don't export
emptyPartition :: (Ix x, Ix y) => (Vertex m x, Vertex m x) -> Bipartition m x n y
emptyPartition pRange = listArray pRange emptyLists
    where emptyLists = (replicate (rangeSize pRange) []) :: [[Vertex n y]]

emptyBipartite :: (Ix x, Ix y) => (Vertex 1 x, Vertex 1 x) -> (Vertex 2 y, Vertex 2 y) -> ArrayBipartite x y
emptyBipartite aRange bRange = ArrayBipartite (emptyPartition aRange) (emptyPartition bRange)

instance (Ix x, Ix y) => Bipartite (Edge 1 x 2 y) (ArrayBipartite x y) where
   insert edge (ArrayBipartite partA partB) = ArrayBipartite (insertPart partA edge) partB

instance (Ix x, Ix y) => Bipartite (Edge 2 y 1 x) (ArrayBipartite x y) where
   insert edge (ArrayBipartite partA partB) = ArrayBipartite partA (insertPart partB edge)
