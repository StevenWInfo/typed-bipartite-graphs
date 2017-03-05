{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module ArrayBipartite
    ( Vertex (..)
    , Bipartite (..)
    , Bipartition
    , Edge
    -- , insertPart -- Don't export
    , genVertex
    , markA
    , markB
    -- , emptyPartition -- Don't export
    , emptyBipartite
    , InsertEdge (insert)
    ) where

import GHC.TypeLits
import Data.Array
import Data.Proxy

-- Vertices with type level Ints might be easier to extend into multipartite graphs if they are possible.
newtype Vertex (n :: Symbol) a = Vertex a deriving (Eq, Ord, Ix, Show)

-- I suppose if both partitions are part of the same graph then maybe they should have the same type of "held value". Also simpler.
-- Might be more useful if they are able to have different types though.
data Bipartite x y = Bipartite (Bipartition "a" x "b" y) (Bipartition "b" y "a" x) deriving (Show)

type Bipartition m x n y = Array (Vertex m x) [Vertex n y]

genVertex :: Proxy (n :: Symbol) -> a -> Vertex n a
genVertex _ a = Vertex a

markA :: Proxy "a"
markA = Proxy

markB :: Proxy "b"
markB = Proxy

-- Applicative and Monad too?
instance Functor (Vertex n) where
    fmap f (Vertex a) = Vertex (f a)

-- Edge first and then bipartite for insert arguments might make more sense. Insert "edge" into "graph"
class InsertEdge edge bp | edge -> bp where
    insert :: edge -> bp -> bp

type Edge m x n y = (Vertex m x, Vertex n y)

-- Don't export
insertPart :: (Ix x, Ix y) => Bipartition m x n y -> Edge m x n y -> Bipartition m x n y
insertPart part (vertM, vertN) = part // [(vertM, vertN : (part ! vertM))]

-- Don't export
emptyPartition :: (Ix x, Ix y) => (Vertex m x, Vertex m x) -> Bipartition m x n y
emptyPartition pRange = listArray pRange emptyLists
    where emptyLists = (replicate (rangeSize pRange) []) :: [[Vertex n y]]

emptyBipartite :: (Ix x, Ix y) => (Vertex "a" x, Vertex "a" x) -> (Vertex "b" y, Vertex "b" y) -> Bipartite x y
emptyBipartite aRange bRange = Bipartite (emptyPartition aRange) (emptyPartition bRange)

instance (Ix x, Ix y) => InsertEdge (Edge "a" x "b" y) (Bipartite x y) where
   insert edge (Bipartite partA partB) = Bipartite (insertPart partA edge) partB

instance (Ix x, Ix y) => InsertEdge (Edge "b" y "a" x) (Bipartite x y) where
   insert edge (Bipartite partA partB) = Bipartite partA (insertPart partB edge)
