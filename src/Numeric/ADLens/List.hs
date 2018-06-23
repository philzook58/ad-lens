{-# LANGUAGE RankNTypes #-}
module Numeric.ADLens.List
    ( sum
    , replicate'
    , repeat'
    , map'
    , zip'
    , unzip'
    , maximum'
    , sort'
    , group2'
    ) where

import Numeric.ADLens.Lens
import Data.List (sort)
import Control.Applicative (ZipList (..))

sum' :: Num a => Lens' [a] a
sum' = lens'' $ \xs -> (sum xs, \dy -> replicate (length xs) dy)

replicate' :: Num a => Int -> Lens' a [a]
replicate' n = lens'' $ \x -> (replicate n x, sum)

repeat' :: Num a => Lens' a [a]
repeat' = lens'' $ \x -> (repeat x, sum)

map' :: Lens' a b -> Lens' [a] [b]
map' l = lens'' $ \xs -> let (bs, fs) = unzip . map (unlens'' l) $ xs in 
                       (bs, getZipList . ((ZipList fs) <*>) . ZipList)

zip' :: Lens' ([a], [b]) [(a,b)]
zip' = lens'' $ \(as,bs) -> (zip as bs, unzip)

unzip' :: Lens' [(a,b)] ([a], [b])
unzip' = lens'' $ \xs -> (unzip xs, uncurry zip)

maximum' :: (Num a, Ord a) => Lens' [a] a
maximum' = lens'' $ \(x:xs) -> let (best, bestind, lenxs) = argmaxixum x 0 1 xs in
                               (best, \dy -> onehot bestind lenxs dy) where
    argmaxixum best bestind len [] = (best, bestind, len) 
    argmaxixum best bestind curind (x:xs) = if x > best then argmaxixum x curind (curind + 1) xs else argmaxixum best bestind (curind + 1) xs  
    onehot n m x | m == 0 = []
                 | n == m = x : (onehot n (m-1) x) 
                 | otherwise = 0 : (onehot n (m-1) x)

sort' :: Ord a => Lens' [a] [a]
sort' = lens'' $ \xs -> let (sxs, indices) = unzip . sort $ zip xs [0 ..] in
                        (sxs, desort indices) where
                          desort indices = snd . unzip . sort . zip indices

group2 :: [a] -> [(a,a)]
group2 (x:y:xs) = (x,y) : group2 xs
group2 [] = []

ungroup2 :: [(a,a)] -> [a]
ungroup2 [] = []
ungroup2 ((x,y):xs) = x : y : ungroup2 xs

group2' :: Lens' [a] [(a,a)]
group2' = lens'' (\xs -> (group2 xs, ungroup2))
