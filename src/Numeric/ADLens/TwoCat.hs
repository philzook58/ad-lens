{-# LANGUAGE ScopedTypeVariables #-}
module Numeric.ADLens.TwoCat where

import Numeric.ADLens.Basic2
{-
The idea here is that we need to keep weights explicitly out to the side

The makes the diagrammatics 2D in a way that is slightly different than just monoidal
      |
    ------
---|      |---
    ------


    Recurrent nuerla nets flip my intended role of weights and inputs.


    
-}

{-  This type is if the weights are only on the input side, but really... -}
-- newtype AD w a b = AD (w -> a -> (b, b -> (a,w)))

type WAD w a b = Lens'' (w,a) b
{-

vertical, horizontal (compose a,b, par on w ), and diagonal composition (par on both with reassociation)
From different combos of first second, par, and compose 

The (w,w') form of composition is equaivlanet to a gadt like
data  = Pure | Compose ::  w ++ w'

To iterate the learning process n times,
we can just plug the weights into each other.


-}
type WAD' w w' a b = Lens'' (w,a) (w',b)
{- For any monoidal category we can constuct this composition -}
compose' :: forall w w' w'' w''' a b c. WAD' w' w'' b c -> WAD' w w''' a b -> WAD' (w',w) (w'',w''') a c
compose' f g = comp f' g' where 
               f' :: Lens'' ((w',r),b) ((w'',r),c)
               f' = (first' swap') `comp` assoc'' `comp` (par' id' f) `comp` assoc' `comp`  (first' swap') 
               g' :: Lens'' ((r,w),a) ((r,w'''),b)
               g' = assoc'' `comp` (par' id' g) `comp` assoc' 
compose :: WAD w' b c -> WAD w a b -> WAD (w',w) a c
compose f g =  h where
                        h ((w',w),a) = let (b, g') = g (w, a) in
                                     let (c, f') = f (w',b) in
                                     let h' c' = let (w'',b') = f' c' in
                                                 let (w''',a') = g' b' in
                                                 ((w'',w'''),a') in
                                     (c, h')


rotate :: WAD' w w' a b -> WAD' a b w w'                                      
rotate f = swap' `comp` f `comp` swap'

vcompose :: WAD' w'  w'' c d -> WAD' w w' a b -> WAD' w w'' (c, a) (d, b)
vcompose f g = rotate (compose' (rotate f)  (rotate g) )                             

{-
diagpar :: WAD' w  w' a b -> WAD' w'' w''' c d -> WAD' (w,w') (w'',w''') (a, c) (b, d)
diagpar = par' + rearrangements
-}

id''' :: WAD' () () a a
id''' = id'

labsorb'' :: WAD' ((),w) w a a
labsorb'' = first' labsorb

labsorb''' :: WAD' w ((),w) a a
labsorb''' = first' labsorb'

wswap' :: WAD' (w,w') (w',w) a a
wswap' = first' swap'

-- rotate:: WAD' w a a w
-- rotate = swap'

liftGuy :: Lens'' a b -> WAD' w w a b
liftGuy = second'

liftGirl :: Lens'' w w' -> WAD' w w' a a
liftGirl = first'

-- and so on
wassoc' = liftGirl assoc' 
wassoc'' = liftGirl assoc'' 

{-
type WFun w w' a b = (w,a) -> (w',b)

compose :: WFun w' w'' b c -> WFun w w''' a b -> WFun (w',w) (w'',w''') a c
compsoe f g = par f g :: ((w',b) , (w,a)) -> ((w'',c) , (w''',b))

(par id f) . par 
compose :: (w,a) -> (w',b)
-}

-- id :: WAD () a a
-- id (_ ,a) = (a, \a' -> (() ,a'))
{-
liftAD :: Lens'' a b -> WAD () a b
liftAD f = \(_,a) -> let (b,j) = f a in
                     let j' b' = (() ,j b) in
                     (b, j') -}

liftAD :: Lens'' a b -> WAD () a b
liftAD f = comp f labsorb

id'' :: WAD () a a
id'' = liftAD id'

wswap :: WAD (w , w') a b -> WAD (w', w) a b
wswap f = comp f (first' swap')

-- liftW :: Lens'' w' w -> Lens w a b -> Lens w' a b
-- liftW n f = \(w',a) -> let  
{-
absorb :: WAD ((),w) a b -> WAD w a b 
absorb f (w,a) = let (b,j) = f (((),w),a) in
                let j' b' = let (w',a') = j b' in (((),w'),a')
-}

lwabsorb' :: WAD ((),w) a b -> WAD w a b
lwabsorb' f = comp f (first' labsorb')



{-
-- involves copying w.
par :: AD w a b -> AD w a' b' -> AD w (a,a') (b,b')
par = compose2

par4 :: AD w a b -> AD w' a' b' -> AD (w,w') (a,a') (b,b')

newtype AD w w' a b = AD (w -> a -> (b, w', w' -> b -> (a, w)))  
type AD' w a b = AD w w a b
type AD' w a b = AD w () a b


I guess it's completey symmatrical
But I feel like I want to break the symmettry

We often don't need to differentate through the inputs a.

From this perspective is isn't weight parametrized functions
it's base point parametrized weighters.
type AD a w b = a -> w -> (b, b -> w)  = a -> Lens w b

compose isn't lens composition. It's composing a with b.
Hmm. But then we do need differentiaon with resepct to a for composability.

compose2 :: AD w' w'' a b -> AD w w' a b -> AD w w'' a b
compose2 (AD f) (AD g) = AD h where
                            h w a = let (b1, w', g') = g w a in -- what do I do with 2 b? we're sunk
                                    let (b2, w'', f') = f w' a in
                                let h' c = let (b', w'') = g' c in
                                        let (a', w''') = f' b' in
                                        (a', (w'',w''')) in
                                (c, h')

type WeightAD = AD w w' -- these can get plugged into

 For weight sharing and stuff 

twiddleweights :: WeightAD w w' -> AD w a b -> AD w' a b 
twiddleweights :: WeightAD w w' -> TwoMorph w w' a b

type TwoMorph w w' a b = AD w a b -> AD w' a b


-- This might be better.two morphs are not supposed to screw with a and b
-- The 4 parameter lens might actually be constrained correctly in this case by polymorhpism.
-- in 2parametr form we can just hand back a, like a jerk.
--type TwoMorph w w' = forall a b. AD w a b -> AD w' a b


I am now fairly unconvinced this actuall forms a two category.
The forall a b. thing kind of makes that much structure seem a bit goofy.

I don't think I ever actually 


--id2 :: TwoMorph w w a b
-- id2 = \x -> x

-- These are all = lift ADLens from Basic2

assoc2 ::  TwoMorph (w, (w', w'')) ((w,w'),w'') a b
swap2 :: TwoMorph (w, w') (w',w) a b
absorb2 :: TwoMorph ((), w) w a b  
absorb2' :: TwoMorph w ((),w) a b  
dup2 :: TwoMorph w (w,w) a b
dup2 (AD f) = AD f' where
              f' (w,w') a = (b, j ) = f w a
                            (b', j') = f w' a 
merge :: TwoMorph (w,w) w a b -- The sense is kind of backwards. This feels like dup?
merge (AD f) = AD g where
               g w a = let (b, f') = f (w,w) a
                                                  

fan2 :: TwoMorph w w' a b -> TwoMorph w w'' a b -> TwoMorph w (w',w'') a b
par2 :: TwoMorph w1 w2 a b -> TwoMorph w3 w4 a b -> TwoMorph (w1,w3) (w2,w4) a b




compose2 :: TwoMorph w' w'' a b -> TwoMorph w w' a b -> TwoMorph w w'' a b
compose2 f g = f . g


compose_horiz :: TwoMorph w1 w2 b c -> TwoMorph w3 w4 a b -> TwoMorph (w1,w3) (w2,w4) a c
compose_horiz f g (AD x) = AD y where
                            = x $ \(w1,w3) a ->  
                           k = f $ \l1 -> g (\l2 ->  
                           p = g _ 
                           y = compose k p  

compose_horiz :: TwoMorph w1 w2 a b -> TwoMorph w3 w4 a b -> TwoMorph (w1,w3) (w2,w4) a b
compose_horiz f g = f $ \l1 -> g (\l2 ->  

compose :: AD w b c -> AD w' a b -> AD (w,w') a c
compose (AD f) (AD g) = AD h where
                        h (w,w') a = let (b, f') = f w a in
                                     let (c, g') = g w' b in
                                     let h' c = let (b', w'') = g' c in
                                                let (a', w''') = f' b' in
                                                (a', (w'',w''')) in
                                     (c, h')
    -}


