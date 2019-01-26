{-# LANGUAGE RankNTypes #-}
import Numeric.ADLens.Lens
import Numeric.ADLens.Basic
import Numeric.ADLens.List
import Numeric.ADLens.HMatrix
import Numeric.ADLens.Accelerate as Acc


import Numeric.LinearAlgebra


import Data.Array.Accelerate as A
import Data.Array.Accelerate.LLVM.Native  as CPU

{-
dostuff = CPU.run $ dotp (use xs) (use ys)

fromList (Z:.10) [0..] 
-}

type L1 = Matrix Double
type L2 = Matrix Double
type L3 = Matrix Double



type Input = Vector Double
type Output = Vector Double
type Weights = (L1,(L2,(L3,())))

class TupleSum a where
	tupsum :: a -> a -> a
instance TupleSum () where
	tupsum _ _ = ()
instance (Num a, TupleSum b) => TupleSum (a,b) where
	tupsum (a,x) (b,y) = (a + b, tupsum x y)

-- A dense relu neural network example
swaplayer :: Lens' ((Matrix t, b), Vector t) (b, (Matrix t, Vector t))
swaplayer = first' swap' . assoc' 

mmultlayer :: Numeric t => Lens' (b, (Matrix t, Vector t)) (b, Vector t)
mmultlayer = second' mdot'

relulayer :: Lens' (b, Vector Double) (b, Vector Double)
relulayer = second' $ cmap' relu'

uselayer :: Lens' ((Matrix Double, b), Vector Double) (b, Vector Double)
uselayer = swaplayer . mmultlayer . relulayer

runNetwork :: Lens' (Weights, Input) ((), Output)
runNetwork =  uselayer . uselayer . uselayer

main :: IO ()
main = do
   putStrLn "Starting Tests"
   print $ grad (pow' 2) 1
   print $ grad (pow' 4) 1
   print $ grad (map' (pow' 2) . sum') $ [1 .. 5]
   print $ grad (map' (pow' 4) . sum') $ [1 .. 5]
   print $ map (\x -> 4 * x ^ 3 )  [1 .. 5]
   l1 <- randn 3 4
   l2 <- randn 2 3
   l3 <- randn 1 2
   let weights = (l1,(l2,(l3,())))
   print $ view runNetwork (weights, vector [1,2,3,4])
   putStrLn "The neural network gradients"
   print $ set runNetwork (weights, vector [1,2,3,4]) ((), vector [1])




