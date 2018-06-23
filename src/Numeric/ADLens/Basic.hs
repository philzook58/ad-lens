module Numeric.ADLens.Basic
    ( relu'
    , dup'
    , add'
    , sub'
    , div'
    , mul'
    , recip'
    ) where

import Numeric.ADLens.Lens

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




