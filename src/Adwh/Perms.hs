-- # Adwh.Perms
-- Permutations
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
module Adwh.Perms
    where

import Data.List

-- ## perms1 (inductive definition)
{- |
>>> permutations "abc"
["abc","bac","cba","bca","cab","acb"]
>>> perms1 "abc"
["abc","bac","bca","acb","cab","cba"]
-}
perms1 :: [a] -> [[a]]
perms1 = foldr step [[]]
    where
        step = concatMap . inserts

inserts :: a -> [a] -> [[a]]
inserts x = \ case
    []         -> [[x]]
    yys@(y:ys) -> (x:yys) : map (y:) (inserts x ys)

-- ## perms2 (recursive definition)
{- |
>>> perms2 "abc"
["abc","acb","bac","bca","cab","cba"]
-}
perms2 :: [a] -> [[a]]
perms2 = \ case
    [] -> [[]]
    xs -> concatMap subperms (picks xs)
        where
            subperms (x,ys) = map (x:) (perms2 ys)

picks :: [a] -> [(a,[a])]
picks = \ case
    []   -> []
    x:xs -> (x,xs) : [(y,x:ys) | (y,ys) <- picks xs]

