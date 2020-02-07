module Numeric.ADLens.Interval where
import Numeric.Interval

-- interval combinators

type Lens a b = a -> (b, b -> a) 

class Lattice a where
   (/\) :: a -> a -> a
   (\/) :: a -> a -> a
   top :: a
   bottom :: a

-- x /\ x = x
-- 

instance (Lattice a, Lattice b) => Lattice (a,b) where
    (x,y) /\ (a,b) = ( x /\ a  , y /\ b )
    (x,y) \/ (a,b) = ( x \/ a  , y \/ b )
    top = (top, top)
    bottom = (bottom, bottom) -- ? very fishy


instance (Fractional a, Ord a) => Lattice (Interval a) where
    (/\) = intersection
    (\/) = hull
    top = whole
    bottom = empty


instance Lattice () where
    _ /\ _ = ()
    _ \/ _ = ()
    top = () -- fishy
    bottom = () -- fishy



id :: Lattice a =>  Lens a a
id = \x -> (x, \x' -> x  /\  x')
dup :: Lattice a =>  Lens a (a,a)
dup = \x -> ( (x,x), \(x1,x2) ->  x1 /\ x2  )

fuse :: Lattice a => Lens (a,a) a
fuse = \(x,x') -> let x'' = x /\ x' in ( x'' , \x''' -> let x4 = x'' /\ x''' in (x4, x4))  

-- cap is equality
cap :: Lattice a => Lens (a,a) ()
cap = \(x,x') -> ((),  \_ -> let x'' = x /\ x'  in (x'', x''))


posinf :: Fractional a => a
posinf = sup whole
neginf :: Fractional a => a
neginf = inf whole

-- insist that x <= y
leq :: (Ord a, Fractional a) => Lens (Interval a, Interval a) ()
leq = \(x,y) -> (() , \_ ->  (  x /\ (neginf ...  (sup y))  ,  y  /\  ( (inf x) ... posinf) )  )

-- the repetitive-ness of the /\ is a real code smell.
swap :: (Lattice a, Lattice b) => Lens (a,b) (b,a)
swap = \(x,y) -> ( (y,x) , \(y',x') -> (x /\ x', y /\ y' ) )

-- min :: Lens (a,a) a
-- min = \(x,y) -> (min x y, \m -> (x /\ (m + pos)  ,  y /\ (m + pos))

cup :: Lattice a => Lens () (a,a) -- not a very good combinator
cup = \_ -> ( (top, top) ,  \_ -> () )

-- it sure seems like we are doing a lot of unnecessary /\ operations
labsorb :: Lattice a => Lens ((), a) a
labsorb = \(_, x) -> (x, (\x' -> ((), x' /\ x)))
-- rabsorb 

{-
and vice verse

-}



compose f g = \x -> let (y, f' ) =  f x in
                    let (z, g' ) = g y in
                    (z , f' . g')
-- we could do the intersection here.
{-
compose f g = \x -> let (y, f' ) =  f x in
                    let (z, g' ) = g y in
                    (z , \z' -> f' (y /\ (g' z')) /\ x)
                    -}
-- can we though? Does this still work for add?

{-
yeah no, there is something off.
You need to change what you do depending on the number of arguments to the function.
hmm.

-}

const :: a -> Lens () a
const x = \_ -> (x, \_ -> ()) 

{-

Functions and taylor models... ????


-}

fst :: Lattice a => Lens (a,b) a
fst = \(x,y) -> (x, \x' -> (x /\ x', y))
snd :: Lattice b => Lens (a,b) b
snd = \(x,y) -> (y, \y' -> (x, y /\ y'))


-- par :: Lens a b -> Lens c d -> Lens (a,c) (b,d)
-- par f g = 


sum :: (Num a, Lattice a) => Lens (a,a) a
sum = \(x,y) -> ( x + y, \s -> ( x /\ (s - y),  y /\ (s - x)))
mul :: (Fractional a, Lattice a) => Lens (a,a) a
mul = \(x,y) -> (x * y, \m -> (x /\  (m / y)  , y /\ (m / x) ))

div :: (Fractional a, Lattice a) => Lens (a,a) a
div = \(x,y) -> (x / y, \d -> ( (d * y) /\ x ,  ((recip d) * x) /\ y ))

liftunop :: Lattice a => (a -> b) -> (b -> a) -> Lens a b
liftunop f finv = \x -> ( f x , \y -> x /\ (finv y) )

recip' :: (Fractional a, Lattice a) => Lens a a
recip' = liftunop recip recip
sin' :: (Floating a, Lattice a) => Lens a a
sin' = liftunop sin asin
cos' :: (Floating a, Lattice a) => Lens a a
cos' = liftunop cos acos
acos' :: (Floating a, Lattice a) => Lens a a
acos' = liftunop acos cos
exp' :: (Floating a, Lattice a) => Lens a a
exp' = liftunop exp log

-- depending on how library implements sqrt?
sqr :: (Floating a, Lattice a) => Lens a a
sqr = \x -> (x ** 2, \x2 -> x /\ (sqrt x) /\ negate (sqrt x) )

-- sqrt = liftunop ()
pow :: (Floating a, Lattice a) => Lens (a,a) a
pow = \(x,n) -> ( x ** n, \xn -> (x /\ xn ** (recip n),  n /\ (logBase x xn) ))




