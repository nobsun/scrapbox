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
    , lvs
    , lzp
    , dep
    ) where

import Data.List
import Data.Tree
import Data.Monoid
import Data.Set qualified as S

class Functor t => TreeCon t where
    branches :: t a -> [t a]

dfs :: TreeCon t => t a -> [t a]
dfs t = t : concatMap dfs (branches t)

bfs :: TreeCon t => t a -> [t a]
bfs = concat . lvs

lvs :: TreeCon t => t a -> [[t a]]
lvs t = [t] : foldr (lzp . lvs) [] (branches t)

lzp :: Monoid a => [a] -> [a] -> [a]
lzp (x:xs) (y:ys) = x <> y : lzp xs ys
lzp [] ys         = ys
lzp xs []         = xs

dep :: TreeCon t => t a -> Int
dep t 
    | null bs   = 0
    | otherwise = succ $ maximum $ dep <$> bs
    where
        bs = branches t

instance TreeCon [] where
    branches :: [a] -> [[a]]
    branches = singleton . tail

instance TreeCon Tree where
    branches t = t.subForest
