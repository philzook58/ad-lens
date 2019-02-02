{-# LANGUAGE NoImplicitPrelude, TypeSynonymInstances, RankNTypes #-}
module Numeric.ADLens.ContLens where
import Prelude
-- CHAOS REIGNS!

newtype Prox a = Prox (a -> a)
newtype Proj a = Proj (a -> a)

fix' :: Eq a => (a -> a) -> (a -> a)
fix' f x = if x == x' then x' else fix' f x where x' = f x 
-- iterate f x 
proxfix :: Eq a => Prox a -> Proj a
proxfix (Prox f) = Proj $ fix' f 

lower :: Proj a -> Prox a
lower (Proj f) = Prox f

compProx :: Prox a -> Prox a -> Prox a
compProx (Prox f) (Prox g) = Prox (f . g)

-- fanProx :: Prox a -> Prox a -> Prox (a,a) -- Nope
parProx :: Prox a -> Prox b -> Prox (a,b)
parProx (Prox f) (Prox g) = Prox $ \(x,y) -> (f x, g y) 


data Prox' a b = Prox' (a -> b) (b -> a)
data Proj' a b = Proj' (a -> b) (b -> a)

close :: Prox' a b -> Prox a
close (Prox' f g) = Prox (g . f)

fanProx :: Fractional a => Prox' a b -> Prox' a b -> Prox' a (b,b)
fanProx (Prox' f f') (Prox' g g') = Prox' (\x -> (f x, g x)) (\(x,y) -> ((f' x) + (g' y))/2)

avg (x,y) = (x + y) / 2

intersect :: Eq a => Proj a -> Proj a -> Proj a
intersect x y = proxfix $ compProx (lower x) (lower y)
-- what about if proj don't intersect?
-- Proj (a -> a) (Dual a -> Dual a)
-- porjects x to a position in the set, and a halfspace to one that contains all/none of set, at least one?
-- Maybe a convex set should just BE projecting the half space.

-- intersection should return seperating hyperplane
-- interleave projecting half space, and negating under convergence. 

-- if we are in the interior of the set, then the half plane will always move.
-- if we are outside, we are guaranteed that the 
-- if both converge, we are on the boundary
-- 

newtype Dual a = Dual a
data Proj'' a = Proj'' (a -> a) (Dual a -> Dual a)
-- (a -> b) (Dual b -> Dual a), combined with a <-> Dual a

-- If we're thinking projective geometry
-- Then the complement of a point makes sense too
-- The ocmplement does not contain the point unless it is the entire space
-- convex sets ~ Cones in porjective space.
-- minkowski sum = convex hull?

--  It's not really projective geometry. It's cones? Spheres rather than prjective planes?
-- identifying everything that is a positve scalar multiple.
-- data Cone = ?

complement :: Num a => (a,a) -> (a,a)
complement (a,b) = (- a, - b)

--intersect' (Proj f) (Proj g) = Proj $ fix' (\x -> ((f x) + (g x)) / 2))


pos :: (Ord p, Num p) => p -> p
pos x | x >= 0 = x
      | otherwise = 0 

pos' = Proj pos

halfspace :: (Ord b, Num b) => (a -> b) -> a -> b 
halfspace f = pos . f

xaxis :: Num a => Proj' (a,a) a
xaxis = Proj' fst (\x -> (x,0))

yaxis :: Num a => Proj' (a,a) a
yaxis = Proj' snd (\y -> (0,y))

posx :: (Ord a, Num a) => Proj (a,a)
posx = Proj (\(x,y) -> (pos x, y))

type HalfSpace a = (a,a) -> a 
type Point a = (a,a) 
type Plane a = (a,a)

dot :: Num a => (a,a) -> (a,a) -> a
dot (x,y) (a,b) = (x * a) + (y * b)

dual x y = pos (dot x y)