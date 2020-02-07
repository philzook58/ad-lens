{-# LANGUAGE ScopedTypeVariables #-}
module Numeric.ADLens.DiLens where

import Numeric.ADLens.Basic2
{-
The idea here is that we need to keep weights explicitly out to the side.
In the original approach exposed in Spec.hs, we need to pipe around the weights.
This works, but the piping is very repetitive and unclear.
If instead we put the ocmibnation of piping and composition into a new compositions operator
`hcompose`, this complexity is hidden.
This also naturally gives rise to the idea of a composition in the weight direction,
which makes sense for the purposes of weight sharing.


The makes the diagrammatics 2D in a way that is slightly different than just monoidal
      |
    ------
---|      |---
    ------
      |


In a side note 

-}

type WAD' w w' a b = Lens'' (w,a) (w',b)
type WAD'' w a b = WAD' w () a b
{- For any monoidal category we can constuct this composition -}
hcompose :: forall w w' w'' w''' a b c. WAD' w' w'' b c -> WAD' w w''' a b -> WAD' (w',w) (w'',w''') a c
hcompose f g = comp f' g' where 
               f' :: Lens'' ((w',r),b) ((w'',r),c)
               f' = (first' swap') `comp` assoc'' `comp` (par' id' f) `comp` assoc' `comp`  (first' swap') 
               g' :: Lens'' ((r,w),a) ((r,w'''),b)
               g' = assoc'' `comp` (par' id' g) `comp` assoc' 



rotate :: WAD' w w' a b -> WAD' a b w w'                                      
rotate f = swap' `comp` f `comp` swap'

vcompose :: WAD' w'  w'' c d -> WAD' w w' a b -> WAD' w w'' (c, a) (d, b)
vcompose f g = rotate (hcompose (rotate f)  (rotate g) )                             

{-
assoc' :: Lens'' ((a,b),c) (a,(b,c))

assoc'' :: Lens'' (a,(b,c)) ((a,b),c)
-}
diagpar :: forall w w' a b w'' w''' c d. WAD' w  w' a b -> WAD' w'' w''' c d 
           -> WAD' (w,w'') (w',w''') (a, c) (b, d)
diagpar f g = t' `comp` (par' f g) `comp` t where
                t :: Lens'' ((w,w''),(a,c)) ((w,a), (w'',c)) -- yikes. just rearrangements.
                t =  assoc'' `comp` (second' ((second' swap') `comp` assoc' `comp` swap')) `comp` assoc'
                t' :: Lens'' ((w',b), (w''',d)) ((w',w'''),(b,d)) -- the tranpose of t
                t' =  assoc'' `comp` (second'  ( swap'  `comp` assoc'' `comp` (second' swap')))  `comp` assoc'

id''' :: WAD' () () a a
id''' = id'






-- rotate:: WAD' w a a w
-- rotate = swap'

liftIO :: Lens'' a b -> WAD' w w a b
liftIO = second'

liftW :: Lens'' w w' -> WAD' w w' a a
liftW = first'


wassoc' = liftW assoc' 
wassoc'' = liftW assoc'' 

labsorb'' :: WAD' ((),w) w a a
labsorb'' = first' labsorb

labsorb''' :: WAD' w ((),w) a a
labsorb''' = first' labsorb'

wswap' :: WAD' (w,w') (w',w) a a
wswap' = first' swap'

-- and so on

{-
We could also prfitably use a 3 object for 2d fields/ 2d convnets

-}
{-
I should reproduce my dense layer example

type WAD W1 X1 X2
type WAD W2 X2 X3
type WAD 

1-D convnet?
d

exposes paralleism within a layer.
1-D convnet
V2 (V2 (V2 (V2 ))))


Batches?

This technique seems useful also for pumping around strategies/weights.




-}
{-

-}

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


type WAD w a b = Lens'' (w,a) b
compose :: WAD w' b c -> WAD w a b -> WAD (w',w) a c
compose f g =  h where
                        h ((w',w),a) = let (b, g') = g (w, a) in
                                     let (c, f') = f (w',b) in
                                     let h' c' = let (w'',b') = f' c' in
                                                 let (w''',a') = g' b' in
                                                 ((w'',w'''),a') in
                                     (c, h')
{-
Recurrent nueral nets flip my intended role of weights and inputs.
2-d conv-nets are natural 3-dimensional diagrams? 4?
1-d conv-nets are natural
-}

{-  This type is if the weights are only on the input side, but really... -}
-- newtype AD w a b = AD (w -> a -> (b, b -> (a,w)))


{-

vertical, horizontal (compose a,b, par on w ), and diagonal composition (par on both with reassociation)
From different combos of first second, par, and compose 

The (w,w') form of composition is equaivlanet to a gadt like
data  = Pure | Compose ::  w ++ w'

To iterate the learning process n times,
we can just plug the weights into each other.


-}


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


