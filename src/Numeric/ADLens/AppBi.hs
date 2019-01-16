{-# LANGUAGE NoImplicitPrelude, TypeSynonymInstances, RankNTypes #-}
module Numeric.ADLens.AppBi where
-- import Numeric.ADLens.Lens
import Control.Category
import Prelude hiding (id, (.), fst, snd)
import Control.Arrow ((***))

newtype Lens x y = Lens (x -> (y, y -> x)) 
type L s a = Num s => Lens s a

instance Category Lens where
  id = Lens (\x -> (x, id))
  (Lens f) . (Lens g) = Lens $ \x -> let (y, g') = g x in
                                           let (z, f') = f y in
                                           (z, g' . f') 


lift :: Lens a b -> (forall s. Lens s a -> Lens s b)
lift l l' = l . l'

unlift :: (forall s. Lens s a -> Lens s b) -> Lens a b
unlift f = f id


dup :: Num a => Lens a (a,a)
dup = Lens $ \x -> ((x,x), \(dx,dy) -> dx + dy)

par :: Lens a b -> Lens c d -> Lens (a,c) (b,d)
par (Lens f) (Lens g) = Lens l'' where
    l'' (a,c) = ((b,d), f' *** g') where
        (b,f') = f a
        (d,g') = g c

fan :: Num s => Lens s a -> Lens s b -> Lens s (a,b)
fan x y = (par x y) . dup 

-- impkredicative polymorphism errror when we use L in type sig
lift2 :: Lens (a,b) c -> (forall s. Num s => (Lens s a, Lens s b) -> Lens s c)
lift2 l (x,y) = lift l (fan x y)

unlift2 :: (Num a, Num b) => (forall s. (Lens s a, Lens s b) -> Lens s c) -> Lens (a,b) c
unlift2 f = f (fst, snd)

fst :: Num b => Lens (a,b) a
fst = Lens (\(a,b) -> (a, \ds -> (ds, 0)))

snd :: Num a => Lens (a,b) b
snd = Lens (\(a,b) -> (b, \ds -> (0, ds)))

unit :: Lens s ()
unit = Lens (\s -> ((), const s))
-- swap' :: (Num a, Num b) => forall s. (Lens s a, Lens s b) -> Lens s (b, a)
-- swap' = unlift2 (lift2 id . (\(x,y) -> (y,x)))
{-

-- optimizing with Zero tags
-- and maybe 1 tags?
-- may really not be worth it/ detrimental

data ZeroTag a = Z | NZ a

instance Num (ZeroTag a) where
	Z + a = a
	a + Z = a
	(NZ x) + (NZ y) = NZ (x + y)

	Z * _ = Z
	_ * Z = Z
	(NZ x) * (NZ y) = NZ (x * y)

	fromInteger 0 = Z
	fromInteger x = NZ x

tag :: 
untag :: 	
tag2
untag2

-}


{-
fan' :: Num a => Lens' a b -> Lens' a c -> Lens' a (b,c)
fan' l1 l2 = lens'' f3 where
    f1 = unlens'' l1
    f2 = unlens'' l2
    f3 a = ((b,c), \(db,dc) -> df1 db + df2 dc) where
        (b,df1) = f1 a
        (c,df2) = f2 a
        -}