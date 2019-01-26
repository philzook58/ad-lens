{-# LANGUAGE RankNTypes #-}
module Numeric.ADLens.Basic2
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
    , Lens''
    {-
    , snd'
    , swap'
    , assoc'
    , first'
    , second'
    -}
    ) where

import Control.Arrow ((***))

type Lens'' a b = a -> (b, b -> a)

comp :: (b -> (c, (c -> b))) -> (a -> (b, (b -> a))) -> (a -> (c, (c -> a)))
comp f g x = let (b, dg) = g x in
             let (c, df) = f b in
             (c, dg . df)



relu' :: (Ord a, Num a) => Lens'' a a
relu' = \x -> (frelu x, brelu x) where
        frelu x | x > 0 = x
                | otherwise = 0
        brelu x dy | x > 0 = dy
                   | otherwise = 0

add' :: Num a => Lens'' (a,a) a 
add' = \(x,y) -> (x + y, \ds -> (ds, ds))

dup' :: Num a => Lens'' a (a,a)
dup' = \x -> ((x,x), \(dx,dy) -> dx + dy)

sub' :: Num a => Lens'' (a,a) a 
sub' = \(x,y) -> (x - y, \ds -> (ds, -ds))

mul' :: Num a => Lens'' (a,a) a 
mul' = \(x,y) -> (x * y, \dz -> (dz * y, x * dz))

recip' :: Fractional a => Lens'' a a 
recip' = \x-> (recip x, \ds -> - ds / (x * x))

div' :: Fractional a => Lens'' (a,a) a 
div' = (\(x,y) -> (x / y, \d -> (d/y,-x*d/(y * y))))

sin' :: Floating a => Lens'' a a
sin' = \x -> (sin x, \dx -> dx * (cos x))

cos' :: Floating a => Lens'' a a
cos' = \x -> (cos x, \dx -> -dx * (sin x))

pow' :: Num a => Integer -> Lens'' a a
pow' n = \x -> (x ^ n, \dx -> (fromInteger n) * dx * x ^ (n-1)) 

--cmul :: Num a => a -> Lens' a a
--cmul c = lens (* c) (\x -> \dx -> c * dx)

exp' :: Floating a => Lens'' a a
exp' = \x -> let ex = exp x in
                      (ex, \dx -> dx * ex)

fst' :: Num b => Lens'' (a,b) a
fst' = (\(a,b) -> (a, \ds -> (ds, 0)))

snd' :: Num a => Lens'' (a,b) b
snd' = (\(a,b) -> (b, \ds -> (0, ds)))

swap' :: Lens'' (a,b) (b,a)
swap' = (\(a,b) -> ((b,a), \(db,da) -> (da, db)))

assoc' :: Lens'' ((a,b),c) (a,(b,c))
assoc' = \((a,b),c) -> ((a,(b,c)), \(da,(db,dc)) -> ((da,db),dc))
{-
par' :: Lens' a b -> Lens' c d -> Lens' (a,c) (b,d)
par' l1 l2 = f3 where
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
-}
