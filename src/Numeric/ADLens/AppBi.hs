{-# LANGUAGE NoImplicitPrelude, TypeSynonymInstances, RankNTypes #-}
module Numeric.ADLens.AppBi where
-- import Numeric.ADLens.Lens
import Control.Category
import Prelude hiding (id, (.))
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
unlift2 f = f (fst', snd')

fst' :: Num b => Lens (a,b) a
fst' = Lens (\(a,b) -> (a, \ds -> (ds, 0)))

snd' :: Num a => Lens (a,b) b
snd' = Lens (\(a,b) -> (b, \ds -> (0, ds)))

unit :: Lens s () -- ? This isn't right.
unit = Lens (\s -> ((), const s))

add :: Num a => Lens (a,a) a 
add = Lens $ \(x,y) -> (x + y, \ds -> (ds, ds))

sub :: Num a => Lens (a,a) a 
sub = Lens $ \(x,y) -> (x - y, \ds -> (ds, -ds))

mul :: Num a => Lens (a,a) a 
mul = Lens $ \(x,y) -> (x * y, \dz -> (dz * y, x * dz))

recip' :: Fractional a => Lens a a 
recip' = Lens $ \x-> (recip x, \ds -> - ds / (x * x))

div :: Fractional a => Lens (a,a) a 
div = Lens $ (\(x,y) -> (x / y, \d -> (d/y,-x*d/(y * y))))

-- or rather we might define add = unlift2 (+)
instance (Num s, Num a) => Num (Lens s a) where
	x + y = (lift2 add) (x,y)
	x - y = (lift2 sub) (x,y)
	abs = error "TODO"

{-
-- I need a slot to put a functor. Suspicious. Maybe van Laarhoven representaiton, profunctor? might do something cool here. 
newtype Lens f s g a = Lens (f s -> (g a, g a -> f s)) 
-- 

Nope. these don't make sense
instance Applicative f => Functor (Lens s f ) ? Maybe if f has more power than just Functor. Applicative makes sense because of the applicative style we're using
instance Traversable ?

-}
-- quite confusing.
{-
lensmap :: Applicative f => Lens a b -> Lens s (f a) -> Lens s (f b)
lensmap (Lens f) (Lens x) = Lens $ \s -> let (fa, j) = x s in
										 let fbb = fmap f fa in
										 let fb = fmap fst fbb in
										 let fb2s = fmap snd fbb in
										 (fb, \fb' -> j (fb2s <*> fb'))
-}
lensmap :: Applicative f => Lens a b -> Lens (f a) (f b)
lensmap (Lens f) = Lens $ \fa ->
								let fbb = fmap f fa in
								let fb = fmap fst fbb in
								let fb2s = fmap snd fbb in
								(fb, \fb' -> fb2s <*> fb')

-- mappend' :: Monoid m => Lens (m,m) m
-- mappend' = Lens $ \(m1,m2) -> (m1 <> m2, \m -> (m,m)) -- ? Doesn't really make much sense
-- Group g => m1 <> m2, \m -> ( , )
-- list append is the canonical example. This is not always going to make sense probably.
 


-- (Len s' a -> Lens s' b) -> (Lens s (f a)) -> Lens s (f b)
-- Lens a b -> Lens (f a) (f b)
-- Then we can transform into the other forms via lift/unlift

-- ltraverse :: (Applicative f, Applicative t) => Lens a (f b) -> Lens (t a) (f (t b))
-- ltraverse (Lens f) = Lens $ \ta -> let  f 
lsequenceA :: (Applicative f, Applicative t, Traversable f, Traversable t) => Lens (t (f a)) (f (t a))
lsequenceA = Lens $ \tfa -> (sequenceA tfa, sequenceA)

ltraverse :: (Applicative f, Applicative t, Traversable f, Traversable t) =>
             Lens a (f b) -> Lens (t a) (f (t b))
ltraverse f = lsequenceA . (lensmap f)


-- lfoldr :: Lens (a,b) b -> b -> Lens (t a) b
-- lfoldr (Lens f) = Lens $ \(b, ta) -> 
-- lpure :: Lens a (f a) -- gonna need comonadic powers?
-- lpure 
-- 

-- huh. This is just fmap
-- liftA' :: Lens (a,b) c -> Lens (f (a,b)) (f c)
-- liftA' (Lens f) = Lens $ \fab ->

-- I'm not going to be able to allow raw sequence/fmap functions. :(
-- (a -> b) just doesn't have gradient info
-- unless (a -> b) is implcitly (Lens s a') -> Lens a b'? Can I enforce that?
-- Lens f s a = Lens s (f a) 
-- Lens' s a = Lens Id s a



{-
par'' :: Applicative f => f a -> f b -> f (a,b)
par'' x y = (,) <*> x <$> y

unpar'' :: Applicative f =>  f (a,b) -> (f a, f b) -- unlift?
par'' x = (fmap fst x, fmap snd x)
-}
-- auto-unlift?

-- Interesting similarity to Le's backprop BVar. He uses the parameter s as the wengert tape. proteected like in the ST monad.
-- Not really the intepretation here, but the types do look mighty similar.

-- if we don't want to update the input we can work with
-- x -> Lens w y
-- we can always take parameters out of the input
-- A curious combination of currying and functor into (->)
-- via laziness, it is conceivable that the graident of x will not be computed.
ungrad :: Lens (a,b) c -> (a -> Lens b c)
ungrad (Lens f) a = Lens (\b -> let (c,j) = f (a,b) in (c, snd . j))

-- swap' :: (Num a, Num b) => forall s. (Lens s a, Lens s b) -> Lens s (b, a)
-- swap' = unlift2 (lift2 id . (\(x,y) -> (y,x)))

-- Does Lens have a curry instance?
-- Lens a (Lens b c) -> Lens (a,b) c
-- I'm going to need to give a lens to the backwards pass?
-- Is this what higher order differentiation looks like?
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


