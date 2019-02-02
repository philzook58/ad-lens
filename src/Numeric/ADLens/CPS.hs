{-# LANGUAGE NoImplicitPrelude, TypeSynonymInstances, RankNTypes #-}
module Numeric.ADLens.CPS where
-- import Numeric.ADLens.Lens
import Control.Category
import Prelude hiding (id, (.))
import Control.Arrow ((***))
import Data.Functor.Const
import Data.Traversable
import Linear.V2
--newtype Lens x y = Lens (x -> (y, y -> x)) 


data Dual x = Dual x x -- forward mode
data RDual r x = RDual x (x -> r) -- reverse mode

-- id = \(x,f) -> (x,f)
type Lens' r x y = RDual x r -> RDual y r
-- We can cps/yoneda a regular lens form into this form
-- x -> (y, dy -> dx) = x -> (y, forall r. (dx -> r) -> (dy -> r))
-- = forall r. (x, dx -> r) -> (y, dy -> r)
-- I think that polymorphism forall r. protects y from dependence on dx -> r

-- this matches pretty well the standard form of forward mode

instance (Num r, Num x) => Num (RDual r x) where
    (RDual x j) + (RDual y k) = RDual (x + y) (\x' -> (j x') + (k x'))
    (RDual x j) - (RDual y k) = RDual (x - y) (\x' -> (j x') - (k x'))
    (RDual x j) * (RDual y k) = RDual (x * y) (\x' -> (j (y * x')) + (k (x * x')))
    fromInteger x = RDual (fromInteger x) (const 0)


lift x = RDual x id
lift2 x y = RDual (V2 x y) id
grad (RDual y f) = f 1

q1 = grad $ (\x -> x*x + 7*x) (lift 2) 
q2 = grad $ (\(V2 x y) -> x*y + x*x) $ V2 (RDual 3 (\x -> V2 x 0)) (RDual 2 (\y -> V2 0 y))
-- (RDual 2 (\x -> (x,0)) ) (RDual 3 (\y -> (0,y)))
-- (lift2 2 3)

-- a reasonable alternative to the applicative bidrectional style.

-- sometimes we need RDual r (V2 x) -> V2 (RDual r x) 
-- this is porbably a traversal.

-- data RDual r x = RDual x (x -> (r -> r)) -- reverse mode
-- Then we could just have an update avoiding adding zeros
-- RDual 3 (\x -> \ V2 x' y' -> V2 (x' + x) y')
-- the funny thing is actual lenses have the info needed to solve the adding 0 problem