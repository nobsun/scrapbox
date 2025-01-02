-- # TreeCon
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE LambdaCase, MultiWayIf #-}
{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE DataKinds, PolyKinds, NoStarIsType, TypeFamilyDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot, NoFieldSelectors, DuplicateRecordFields #-}
module TreeCon
    ( TreeCon (..)
    , bfs
    , dfs
    ) where

import Data.Set qualified as S

class Functor t => TreeCon t where
    branches :: t a -> [t a]

dfs :: TreeCon t => t a -> [t a]
dfs t = t : concatMap dfs (branches t)

bfs :: TreeCon t => t a -> [t a]
bfs = concat . levels

levels :: TreeCon t => t a -> [[t a]]
levels t = [t] : foldr (longzw (++) . levels) [] (branches t)

longzw :: (a -> a -> a) -> [a] -> [a] -> [a]
longzw f (x:xs) (y:ys) = f x y : longzw f xs ys
longzw _ [] ys = ys
longzw _ xs [] = xs
