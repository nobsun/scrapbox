-- # ScanList

{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE LambdaCase #-}

module ScanList
    (
    ) where

import Control.Arrow
import Data.Array
import Data.List 
import ScanArray

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

