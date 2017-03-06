{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module ArrayBipartite
    ( ArrayBipartite (..)
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
insertPart :: (Ix x, Ix y) => (Vertex m x, Vertex n y) -> Bipartition m x n y -> Bipartition m x n y
insertPart (vertM, vertN) part = part // [(vertM, vertN : (part ! vertM))]

-- Don't export
emptyPartition :: (Ix x, Ix y) => (Vertex m x, Vertex m x) -> Bipartition m x n y
emptyPartition pRange = listArray pRange emptyLists
    where emptyLists = (replicate (rangeSize pRange) []) :: [[Vertex n y]]

emptyBipartite :: (Ix x, Ix y) => (Vertex 1 x, Vertex 1 x) -> (Vertex 2 y, Vertex 2 y) -> ArrayBipartite x y
emptyBipartite aRange bRange = ArrayBipartite (emptyPartition aRange) (emptyPartition bRange)

instance (Ix a, Ix b) => Bipartite ArrayBipartite a b where
    insert (ToRight v1 v2) (ArrayBipartite partOne partTwo) = ArrayBipartite (insertPart (v1, v2) partOne) partTwo
    insert (ToLeft v1 v2) (ArrayBipartite partOne partTwo) = ArrayBipartite partOne (insertPart (v2, v1) partTwo)
