-- # BfsArray
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE LambdaCase, MultiWayIf #-}
{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE DataKinds, PolyKinds, NoStarIsType, TypeFamilyDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot, NoFieldSelectors, DuplicateRecordFields #-}
module BfsArray
    (
    ) where

import Control.Arrow
import Data.Array
import Data.Set qualified as S

neighbors :: (Ix i, Enum i) => Array (i,i) a -> (i,i) -> [(i,i)]
neighbors a = \ case
    ij@(i,j) 
        | ij == ij0    -> [                second succ ij,                first succ ij]
        | ij == (i0,w) -> [second pred ij,                                first succ ij]
        | ij == hw     -> [second pred ij,                 first pred ij               ]
        | ij == (h,j0) -> [                second succ ij, first pred ij               ]
        | i  == i0     -> [second pred ij, second succ ij,                first succ ij]
        | j  == j0     -> [                second succ ij, first pred ij, first succ ij]
        | i  == h      -> [second pred ij, second succ ij, first pred ij               ]
        | j  == w      -> [second pred ij,                 first pred ij, first succ ij]
        | otherwise    -> [second pred ij, second succ ij, first pred ij, first succ ij]
        where
            (ij0@(i0,j0),hw@(h,w)) = bounds a

sample :: Array (Int,Int) Int
sample = listArray ((1,1),(9,9)) [ i*j | (i,j) <- range ((1,1),(9,9))]

sample2 :: Array (Int,Int) [(Int,Int)]
sample2 = listArray (bounds sample) (neighbors sample <$> range (bounds sample))





