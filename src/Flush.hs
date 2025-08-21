-- # Flush
-- ABC418 C
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
module Flush
    ( 
    ) where

-- ## import リスト
import Control.Arrow
import Data.Array
import Data.Bool
import Data.List
import Util

-- ##
{- |
>>> baz [4,1,8,4]
array (1,8) [(1,4),(2,3),(3,3),(4,3),(5,1),(6,1),(7,1),(8,1)]
-}
baz :: [Int] -> Array Int Int
baz xs = accumArray (+) 0 (1,n) (phi =<< xs)
    where
        n = maximum xs
        phi i = map (,1) [1 .. i]
{- |
>>> sample = [4,1,8,4]
>>> map (flush sample) [1,8,5,2,10]
[1,17,14,5,-1]
-}
flush :: [Int] -> (Int -> Int)
flush xs b = bool -1 (succ (ba ! b)) (b <= n)
    where
        n = maximum xs
        phi i = map (,1) [1 .. i]
        aa = accumArray (+) 0 (1,n) (phi =<< xs)
        ba = listArray (1,n) (scanl' (+) 0 (elems aa))



