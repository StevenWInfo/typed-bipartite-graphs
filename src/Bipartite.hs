{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Bipartite
    ( Bipartite (..)
    , Vertex (Vertex)
    , genVertex
    , markOne
    , markTwo
    , Edge
    ) where

import Data.Proxy
import GHC.TypeLits
import Data.Array

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

type Edge m x n y = (Vertex m x, Vertex n y)

class Bipartite edge bp | edge -> bp where
    --empty  :: bp
    insert :: edge -> bp -> bp
    -- empty  :: bp a b
    --insert :: edge -> bp a b -> bp a b
