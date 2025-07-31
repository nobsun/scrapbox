-- # Nexus
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
module Nexus
    (
    ) where

import Data.Bits
import Data.Bool
import Data.List
import Data.Set qualified as S

perms1, perms2 :: [a] -> [[a]]
perms1 = foldr (concatMap . inserts) [[]]

inserts :: a -> [a] -> [[a]]
inserts x = \ case
    []   -> [[x]]
    yys@(y:ys) -> (x:yys) : map (y:) (inserts x ys)

perms2 = \ case
    [] -> [[]]
    xs -> concatMap subperms (picks xs) where
        subperms (x,ys) = map (x:) (perms2 ys)

picks :: [a] -> [(a,[a])]
picks = \ case
    []     -> []
    (x:xs) -> (x,xs) : [ (y,x:ys) | (y,ys) <- picks xs ]

setIfNot :: Bits a => a -> Int -> Maybe a
setIfNot a i = bool (Just $ setBit a i) Nothing (testBit a i)

next :: (Bits a, Ord a) => Int -> a -> S.Set a
next n a = foldl' phi S.empty [0 .. pred n] where
    phi s i = if testBit a i then s else S.insert (setBit a i) s

nexusStep :: (Bits a, Ord a) => Int -> S.Set a -> S.Set a
nexusStep n s = S.unions $ map (next n) $ S.toList s

nexus :: (Bits a, Ord a) => Int -> S.Set a -> [S.Set a]
nexus n s = scanl' (flip nexusStep) s (replicate n n)
