-- # ScanArray

{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE LambdaCase #-}

module ScanArray
    ( scanArray
    ) where

import Control.Arrow ( Arrow(second, first) )
import Data.Array ( Ix(range), (!), assocs, bounds, listArray, Array )

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
