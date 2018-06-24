{-# LANGUAGE RankNTypes #-}
module Numeric.ADLens.Basic
    ( relu'
    , dup'
    , add'
    , sub'
    , div'
    , mul'
    , sin'
    , cos'
    , exp'
    , pow'
    , recip'
    , fst'
    , snd'
    , swap'
    , assoc'
    ) where

import Numeric.ADLens.Lens
import Control.Arrow ((***))

relu' :: (Ord a, Num a) => Lens' a a
relu' = lens'' $ \x -> (frelu x, brelu x) where
        frelu x | x > 0 = x
                | otherwise = 0
        brelu x dy | x > 0 = dy
                   | otherwise = 0

add' :: Num a => Lens' (a,a) a 
add' = lens'' $ \(x,y) -> (x + y, \ds -> (ds, ds))

dup' :: Num a => Lens' a (a,a)
dup' = lens'' $ \x -> ((x,x), \(dx,dy) -> dx + dy)

sub' :: Num a => Lens' (a,a) a 
sub' = lens'' $ \(x,y) -> (x - y, \ds -> (ds, -ds))

mul' :: Num a => Lens' (a,a) a 
mul' = lens'' $ \(x,y) -> (x * y, \dz -> (dz * y, x * dz))

recip' :: Fractional a => Lens' a a 
recip' = lens'' $ \x-> (recip x, \ds -> - ds / (x * x))

div' :: Fractional a => Lens' (a,a) a 
div' = lens'' $ (\(x,y) -> (x / y, \d -> (d/y,-x*d/(y * y))))

sin' :: Floating a => Lens' a a
sin' = lens'' $ \x -> (sin x, \dx -> dx * (cos x))

cos' :: Floating a => Lens' a a
cos' = lens'' $ \x -> (cos x, \dx -> -dx * (sin x))

pow' :: Num a => Integer -> Lens' a a
pow' n = lens'' $ \x -> (x ^ n, \dx -> (fromInteger n) * dx * x ^ (n-1)) 

--cmul :: Num a => a -> Lens' a a
--cmul c = lens (* c) (\x -> \dx -> c * dx)

exp' :: Floating a => Lens' a a
exp' = lens'' $ \x -> let ex = exp x in
                      (ex, \dx -> dx * ex)

fst' :: Num b => Lens' (a,b) a
fst' = lens'' (\(a,b) -> (a, \ds -> (ds, 0)))

snd' :: Num a => Lens' (a,b) b
snd' = lens'' (\(a,b) -> (b, \ds -> (0, ds)))

swap' :: Lens' (a,b) (b,a)
swap' = lens'' (\(a,b) -> ((b,a), \(db,da) -> (da, db)))

assoc' :: Lens' ((a,b),c) (a,(b,c))
assoc' = lens'' $ \((a,b),c) -> ((a,(b,c)), \(da,(db,dc)) -> ((da,db),dc))

par' :: Lens' a b -> Lens' c d -> Lens' (a,c) (b,d)
par' l1 l2 = lens'' f3 where
    f1 = unlens'' l1
    f2 = unlens'' l2
    f3 (a,c) = ((b,d), df1 *** df2) where
        (b,df1) = f1 a
        (d,df2) = f2 c

fan' :: Num a => Lens' a b -> Lens' a c -> Lens' a (b,c)
fan' l1 l2 = lens'' f3 where
    f1 = unlens'' l1
    f2 = unlens'' l2
    f3 a = ((b,c), \(db,dc) -> df1 db + df2 dc) where
        (b,df1) = f1 a
        (c,df2) = f2 a

first' :: Lens' a b -> Lens' (a, c) (b, c)
first' l = par' l id

second' :: Lens' a b -> Lens' (c, a) (c, b)
second' l = par' id l
