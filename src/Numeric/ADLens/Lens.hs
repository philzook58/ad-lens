{-# LANGUAGE RankNTypes #-}
module Numeric.ADLens.Lens
    ( Lens'
    , lens''
    , unlens'' 
    , constlens
    , set
    , view
    , grad 
    ) where
        
import Data.Functor.Identity
import Data.Functor.Const
import Data.Functor.Compose

type Lens' a b = forall f. Functor f => (b -> f b) -> a -> f a

lens'' :: (a -> (b, b -> a)) -> Lens' a b
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

constlens :: Lens' (a,b) c -> b -> Lens' a c
constlens l b = lens'' $ \a -> let (c, df) = f (a,b) in
                             (c, fst . df) where 
                                           f = unlens'' l


grad :: Num b => Lens' a b -> a -> a
grad l = (flip (set l)) 1 


