{-# LANGUAGE RankNTypes, FlexibleContexts #-}
module Numeric.ADLens.HMatrix
    ( 
    
    ) where

import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Devel (zipVectorWith)
import Numeric.ADLens.Lens
-- import Data.Vector as V

dot' :: (Container Vector t, Numeric t) => Lens' (Vector t, Vector t) t
dot' = lens'' $ \(v1,v2) -> (v1 <.> v2, \ds -> (scale ds v2, scale ds v1))

mdot' :: (Product t, Numeric t) => Lens' (Matrix t, Vector t) (Vector t)
mdot' = lens'' $ \(a,v) -> (a #> v, \dv -> (outer dv v, dv <# a))

add' :: Additive c => Lens' (c, c) c
add' = lens'' $ \(v1,v2) -> (add v1 v2, \dv -> (dv, dv))

-- I need konst I think?
sumElements' :: (Container Vector t, Numeric t) => Lens' (Vector t) t
sumElements' = lens'' $ \v -> (sumElements v, \ds -> scalar ds)

reshape' :: Container Vector t => Int -> Lens' (Vector t) (Matrix t)
reshape' n = lens'' $ \v -> (reshape n v,  \dm -> flatten dm)

-- conjugate transpose not trace
tr'' ::  (Transposable m mt, Transposable mt m) => Lens' m mt
tr'' = lens'' $ \x -> (tr x, \dt -> tr dt)


flatten' :: (Num t, Container Vector t) => Lens' (Matrix t) (Vector t)
flatten' = lens'' $ \m -> let s = fst $ size m in  
                          (flatten m,  \dm -> reshape s dm)


norm_2' :: (Container c R, Normed (c R), Linear R c) => Lens' (c R) R
norm_2' = lens'' $ \v -> let nv = norm_2 v in (nv, \dnv -> scale (2 * dnv / nv) v )



cmap' :: (Element b, Container Vector e) => (Lens' e b) -> Lens' (Vector e) (Vector b)
cmap' l = lens'' $ \c -> (cmap f c, \dc -> zipVectorWith f' c dc) where
        f = view l
        f' = set l
 
{-
maxElement' :: Container c e => Lens' (c e) e
maxElement' = lens'' $ \v -> let i = maxIndex v in (v ! i, dv -> scalar 0)
-}

det' :: Field t => Lens' (Matrix t) t
det' = lens'' $ \m -> let (minv, (lndet, phase)) = invlndet m in
                    let detm = phase * exp detm in
                    (detm, \ds -> (scale (ds * detm) minv))

diag' :: (Num a, Element a) => Lens' (Vector a) (Matrix a)
diag' = lens'' $ \v -> (diag v, takeDiag)

takeDiag' :: (Num a, Element a) => Lens' (Matrix a) (Vector a) 
takeDiag' = lens'' $ \m -> (takeDiag m, diag)
