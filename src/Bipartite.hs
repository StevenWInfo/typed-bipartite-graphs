{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}

module Bipartite
    ( Bipartite (..)
    , Vertex (Vertex)
    , genVertex
    , markOne
    , markTwo
    , Edge (..)
    ) where

import Data.Proxy
import GHC.TypeLits
import Data.Array

{-

The problem that I initially wanted to solve was that there wasn't anything at the type level that was enforcing Bipartite invariants. I created a sort of solution that only does it at the type level, but doesn't have anything at the value level to let you know which partition a vertex should go to. Perhaps it would make sense to have both, but then I feel like the type level value isn't doing anything. Also, what's the point of distinguishing them by type if I just want to disregard it when inserting an edge anyways? I guess the main thing is to enforce the partition constraint on edges going from one partition to the other.

I could create multiple edge types. I'm creating multiple vertex types anyways. That could get pretty unwieldy though.

Perhaps I could use type level lists to create multipartite graphs.

Perhaps the bipartite type literals should be 0 and 1 instead of 1 and 2.

 -}

newtype Vertex (n :: Nat) a = Vertex a deriving (Eq, Ord, Show, Ix)

-- Applicative and Monad too?
instance Functor (Vertex n) where
    fmap f (Vertex a) = Vertex (f a)

genVertex :: Proxy (n :: Nat) -> a -> Vertex n a
genVertex _ a = Vertex a

markOne :: Proxy 1
markOne = Proxy

markTwo :: Proxy 2
markTwo = Proxy

data Edge m x n y = ToRight (Vertex m x) (Vertex n y) | ToLeft (Vertex m x) (Vertex n y) deriving (Show, Eq, Ord)

type BPEdge x y = Edge 1 x 2 y

-- The Ord constraint actually doesn't make sense outside of things like Map implementation. Might have to create wrapper types for Map or something to get rid of this. Same for Ix.
-- empty would be nice, but possibly doesn't exactly work with types like arrays.
class Bipartite bp where
    -- empty :: bp a b
    insert :: (Ord a, Ord b, Ix a, Ix b) => BPEdge a b -> bp a b -> bp a b
