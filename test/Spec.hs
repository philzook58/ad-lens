{-# LANGUAGE RankNTypes #-}
import Numeric.ADLens.Lens
import Numeric.ADLens.Basic
import Numeric.ADLens.List


main :: IO ()
main = do
   putStrLn "Starting Tests"
   print $ grad (pow' 2) 1
   print $ grad (pow' 4) 1
   print $ grad (map' (pow' 2) . sum') $ [1 .. 5]
   print $ grad (map' (pow' 4) . sum') $ [1 .. 5]
   print $ map (\x -> 4 * x ^ 3 )  [1 .. 5]
   


