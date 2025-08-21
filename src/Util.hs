-- # Util
-- 
-- ## 言語拡張と`module`宣言
-- 
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE LambdaCase, MultiWayIf #-}
{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE DataKinds, PolyKinds, NoStarIsType, TypeFamilyDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot, NoFieldSelectors, DuplicateRecordFields #-}
module Util
    where

--

scons :: a -> [a] -> [a]
scons !x xs = x : xs

smap :: (a -> b) -> [a] -> [b]
smap f = \ case
    x:xs -> scons (f x) (smap f xs)
    []   -> []

spanCount :: (a -> Bool) -> [a] -> (Int, [a])
spanCount p = \ case
    x:xs | p x -> case spanCount p xs of
        (n,ys)    -> (succ n, ys)
    xs         -> (0,xs)

type RLE a = [(a, Int)]

toRLE :: Eq a => [a] -> RLE a
toRLE = \ case
    []   -> []
    x:xs -> case spanCount (x ==) xs of
        (n,ys) -> (x, succ n) : toRLE ys

fromRLE :: RLE a -> [a]
fromRLE = (uncurry (flip replicate) =<<)

para :: (a -> ([a], b) -> b)  -> b -> [a] -> b
para phi z = \ case
    a:as -> phi a (as, para phi z as)
    []   -> z

type SzL a = ([a], Int)

toSzL :: [a] -> SzL a
toSzL xs = (xs, length xs)

consSzL :: a -> SzL a -> SzL a
consSzL x (xs, n) = (x:xs, succ n)

appendSzL :: SzL a -> SzL a -> SzL a
appendSzL (xs,m) (ys,n) = (xs++ys, m+n)

