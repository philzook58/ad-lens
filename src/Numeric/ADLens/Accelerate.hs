{-# LANGUAGE RankNTypes, FlexibleContexts #-}
module Numeric.ADLens.Accelerate
    ( 
    ) where

import Data.Array.Accelerate as A
import Numeric.ADLens.Basic2 (Lens'')

--type Lens'' a b = a -> (b, b -> a)

add :: A.Shape b => Lens'' (Acc (Array b Double), Acc (Array b Double)) (Acc (Array b Double))
add = \(xs,ys) -> (A.zipWith (+) xs ys, \dz -> (dz,dz))

--re

--reshape :: (A.Shape sh, A.Shape sh1) => Lens'' (Acc (Array sh Double)) (Acc (Array sh2 Double))
--reshape sh = \x -> (A.reshape)
