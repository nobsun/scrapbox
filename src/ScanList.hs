-- # ScanList

{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE LambdaCase #-}

module ScanList
    (
    ) where

import Control.Arrow
import Data.Array
import Data.List 

bft :: (a -> a -> a) -> ([a] -> [a]) -> ([a] -> [a]) -> a -> [[a]]
bft cmb phi psi a = lvs where
    lvs = [a] : [ zipWith cmb qs rs 
                | ps <- lvs
                , let qs = phi ps
                , let rs = psi ps ]

pascal :: [[Integer]]
pascal = bft (+) phi psi 1 where
    phi = (0:)
    psi = reverse . (0 :) . reverse

indexLvs :: Int -> [[(Int,Int)]]
indexLvs i = bft cmb phi psi (i,i) where
    phi ij = (succ *** pred) (head ij) : ij
    psi ij = reverse $ (pred *** succ) (head ji) : ji where
        ji = reverse ij
    cmb (p,q) (s,t) = (max p s, max q t)


nCr :: Int -> Int -> Integer
nCr n r = pascal !! n !! r


scanArray :: (Ix i, Enum i)
          => (a -> b)
          -> (a -> b -> b)
          -> (a -> b -> b -> b)
          -> Array (i,i) a -> Array (i,i) b
scanArray f g h sa = ta where
    ta  = listArray (bounds sa) (phi <$> assocs sa)
    phi = \ case
        (ij@(i,j),a)
            | ij == ij0 -> f a
            | i  == i0  -> g a (ta ! second pred ij)
            | j  == j0  -> g a (ta ! first  pred ij)
            | otherwise -> h a (ta ! first  pred ij) (ta ! second pred ij)
            where
                ij0 = fst (bounds sa)
                i0  = fst ij0
                j0  = snd ij0

{- | 
>>> minpath sample (100,100)
20099
-}
minpath :: Array (Int,Int) Int -> (Int,Int) -> Int
minpath = (!) . scanArray id (+) (\ x y z -> x + y `min` z)

sample :: Array (Int,Int) Int
sample = listArray ((1,1),(100,100)) (uncurry (+) <$> range ((1,1),(100,100)))
