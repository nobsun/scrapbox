-- # ScanArray

{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE LambdaCase #-}

module ScanArray
    ( scanArray
    ) where

import Control.Arrow
import Data.Array

scanArray :: (Ix i, Enum i)
          => (a -> b)
          -> (b -> a -> b)
          -> (b -> b -> b -> a -> b)
          -> Array (i,i) a -> Array (i,i) b
scanArray f g h sa = ta where
    ta  = listArray (bounds sa) (phi <$> assocs sa)
    phi = \ case
        (ij@(i,j),a)
            | ij == ij0 -> f a
            | i  == i0  -> g (ta ! second pred ij) a
            | j  == j0  -> g (ta ! first  pred ij) a
            | otherwise -> h (ta ! (pred *** pred) ij) (ta ! first  pred ij) (ta ! second pred ij) a
            where
                ij0 = fst (bounds sa)
                i0  = fst ij0
                j0  = snd ij0

{- | 
>>> minpath sample (100,100)
20099
-}
minpath :: Array (Int,Int) Int -> (Int,Int) -> Int
minpath sa = (scanArray f g h sa !) where
    f = id
    g = (+)
    h _ a b c = a `min` b + c

sample :: Array (Int,Int) Int
sample = listArray ((1,1),(100,100)) (uncurry (+) <$> range ((1,1),(100,100)))

{- | 
>>> elems $ cumulativeSum2D smallSample
[2,2,2,7,8,3,3,6,11,12,3,11,19,24,27,7,16,24,29,38,7,25,35,47,56]
-}
cumulativeSum2D :: Num a => Array (Int,Int) a -> Array (Int,Int) a
cumulativeSum2D = scanArray f g h where
    f = id
    g = (+)
    h a b c d = subtract a (b + c + d)

smallSample :: Array (Int,Int) Int
smallSample = listArray ((1,1),(5,5))
            [2,0,0,5,1
            ,1,0,3,0,0
            ,0,8,5,0,2
            ,4,1,0,0,6
            ,0,9,2,7,0]

