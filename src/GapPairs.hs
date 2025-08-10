-- # GapPairs
-- 指定した数以上の間隔をもつ対（間隔付き）のリスト
-- 
-- ## 言語拡張と`module`宣言
-- 最低限の指定をしてある
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE LambdaCase, MultiWayIf #-}
{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE DataKinds, PolyKinds, NoStarIsType, TypeFamilyDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot, NoFieldSelectors, DuplicateRecordFields #-}
module GapPairs
    ( gapPairs
    ) where

import Data.List
{- |
>>> gapPairs 2 "abcde"
[(('a','c'),2),(('a','d'),3),(('a','e'),4),(('b','d'),2),(('b','e'),3),(('c','e'),2)]
-}
gapPairs :: Int -> [a] -> [((a,a),Int)]
gapPairs gap xs
    = concat [ [((x,y), g) | (y, g) <- zip ys [gap ..]]
             | (x,ys) <- zip (take (len - gap) xs) (tails (drop gap xs))
             ]
    where
        len = length xs
