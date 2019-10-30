{-# LANGUAGE NoImplicitPrelude, TypeSynonymInstances, RankNTypes #-}
module Numeric.ADLens.RLens where
-- import Numeric.ADLens.Lens
import Control.Category
import Prelude hiding (id, (.))
import Control.Arrow ((***))

-- recursive Lens. 
-- Does not make sense as automatic derivative.
-- Interesting as a lens-like control structure though.

data RLens a b = RLens (a -> (b, RLens b a)) --  RLens (a -> (b, b -> (a, RLens a b)))
--
compose :: RLens b c -> RLens a b -> RLens a c
compose (RLens f) (RLens g) = RLens $ \x -> let (b, g') = g x in
                                            let (c, f') = f b in
                                            (c, compose g' f')
{-
compose :: RLens b c -> RLens a b -> RLens a c
compose (RLens f) (RLens g) = RLens $ \x -> let (b, g') = g x in
                                            let (c, f') = f b in
                                            (c, \c' -> let (b' , f'') = f' c' in
                                                           (a', g'') = g' b'  in 
                                                            (compose f'' g'')) 
                                                            -}