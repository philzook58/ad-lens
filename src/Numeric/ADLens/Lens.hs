{-# LANGUAGE RankNTypes #-}
module Numeric.ADLens.Lens
    ( Lens'
    , lens''
    , unlens'' 
    
    ) where
        
import Data.Functor.Identity
import Data.Functor.Const
import Data.Functor.Compose

type Lens' a b = forall f. Functor f => (b -> f b) -> a -> f a

lens'' :: Functor f => (a -> (b, b -> a)) -> (b -> f b) -> a -> f a
lens'' h g x = fmap j fb where
    (b, j) = h x
    fb = g b

over :: Lens' a b -> ((b -> b) -> a -> a)
over l f = runIdentity . l (Identity . f)

set :: Lens' a b -> a -> b -> a
set l = flip (\x -> (over l (const x)))

view :: Lens' a b -> a -> b
view l = getConst . l Const

unlens'' :: Lens' a b -> (a -> (b, b -> a))
unlens'' l = getCompose . l (\b -> Compose (b, id))