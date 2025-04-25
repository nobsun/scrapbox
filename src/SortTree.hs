{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE LambdaCase, MultiWayIf #-}
{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE DataKinds, PolyKinds, NoStarIsType, TypeFamilyDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot, NoFieldSelectors, DuplicateRecordFields #-}
module SortTree where

import Control.Arrow
import Data.Function
import Data.List
import Data.List.Extra
import Data.Maybe
import Data.Ord
import Data.Tree
import Data.Tuple

type Cand = [Int]
type Comp = (Int,Int)

initCands :: Int -> [Cand]
initCands n = permutations $ take n $ [0 ..]

initComps :: Int -> [Comp]
initComps n = map toTuple $ combinations 2 $ take n $ [0 ..]

{- --
genTree :: (Comp,[Comp],[Cand]) -> Tree (Comp,[Comp],[Cand])
genTree r@(_,cs,css) = Node r subs where
    subs | length css < 2 = []
         | otherwise = case selectComp css cs of
            (cmp,(lcss,rcss)) -> [genTree (cmp,delete cmp cs,lcss)
                                 ,genTree (swap cmp,delete cmp cs,rcss)]
-- -}
{- |
>>> selectComp (initCands 5) (initComps 5)
((0,1),([[0,1,2,3,4],[2,0,1,3,4],[0,2,1,3,4],[3,0,1,2,4],[0,3,1,2,4],[0,1,3,2,4],[3,0,2,1,4],[0,3,2,1,4],[0,2,3,1,4],[3,2,0,1,4],[2,3,0,1,4],[2,0,3,1,4],[4,0,1,2,3],[0,4,1,2,3],[0,1,4,2,3],[0,1,2,4,3],[4,0,2,1,3],[0,4,2,1,3],[0,2,4,1,3],[0,2,1,4,3],[4,2,0,1,3],[2,4,0,1,3],[2,0,4,1,3],[2,0,1,4,3],[4,0,3,2,1],[0,4,3,2,1],[0,3,4,2,1],[0,3,2,4,1],[4,3,0,2,1],[3,4,0,2,1],[3,0,4,2,1],[3,0,2,4,1],[4,3,2,0,1],[3,4,2,0,1],[3,2,4,0,1],[3,2,0,4,1],[4,0,2,3,1],[0,4,2,3,1],[0,2,4,3,1],[0,2,3,4,1],[4,2,0,3,1],[2,4,0,3,1],[2,0,4,3,1],[2,0,3,4,1],[4,2,3,0,1],[2,4,3,0,1],[2,3,4,0,1],[2,3,0,4,1],[4,0,3,1,2],[0,4,3,1,2],[0,3,4,1,2],[0,3,1,4,2],[4,3,0,1,2],[3,4,0,1,2],[3,0,4,1,2],[3,0,1,4,2],[4,0,1,3,2],[0,4,1,3,2],[0,1,4,3,2],[0,1,3,4,2]],[[1,0,2,3,4],[2,1,0,3,4],[1,2,0,3,4],[3,2,1,0,4],[2,3,1,0,4],[2,1,3,0,4],[3,1,2,0,4],[1,3,2,0,4],[1,2,3,0,4],[3,1,0,2,4],[1,3,0,2,4],[1,0,3,2,4],[4,3,2,1,0],[3,4,2,1,0],[3,2,4,1,0],[3,2,1,4,0],[4,2,3,1,0],[2,4,3,1,0],[2,3,4,1,0],[2,3,1,4,0],[4,1,2,3,0],[1,4,2,3,0],[1,2,4,3,0],[1,2,3,4,0],[4,2,1,3,0],[2,4,1,3,0],[2,1,4,3,0],[2,1,3,4,0],[4,1,3,2,0],[1,4,3,2,0],[1,3,4,2,0],[1,3,2,4,0],[4,3,1,2,0],[3,4,1,2,0],[3,1,4,2,0],[3,1,2,4,0],[4,1,0,2,3],[1,4,0,2,3],[1,0,4,2,3],[1,0,2,4,3],[4,1,2,0,3],[1,4,2,0,3],[1,2,4,0,3],[1,2,0,4,3],[4,2,1,0,3],[2,4,1,0,3],[2,1,4,0,3],[2,1,0,4,3],[4,3,1,0,2],[3,4,1,0,2],[3,1,4,0,2],[3,1,0,4,2],[4,1,0,3,2],[1,4,0,3,2],[1,0,4,3,2],[1,0,3,4,2],[4,1,3,0,2],[1,4,3,0,2],[1,3,4,0,2],[1,3,0,4,2]]))
-}
selectComp :: [Cand] -> [Comp] -> (Comp,([Cand],[Cand]))
selectComp = \ case
    []  -> const $ error "selectComp: empty candidates"
    cds -> \ case
        []  -> error "selectComp: empty comparing pairs"
        cps -> minimumBy (comparing (uncurry max . (length *** length) . snd)) (map ((,) <*> flip prune cds) cps)

{- |
>>> prune (0,1) (initCands 4)
([[0,1,2,3],[2,0,1,3],[0,2,1,3],[3,0,1,2],[0,3,1,2],[0,1,3,2],[3,0,2,1],[0,3,2,1],[0,2,3,1],[3,2,0,1],[2,3,0,1],[2,0,3,1]],[[1,0,2,3],[2,1,0,3],[1,2,0,3],[3,2,1,0],[2,3,1,0],[2,1,3,0],[3,1,2,0],[1,3,2,0],[1,2,3,0],[3,1,0,2],[1,3,0,2],[1,0,3,2]])
-}
prune :: Comp -> [Cand] -> ([Cand],[Cand])
prune (m,n) = partition phi where
    phi c = elemIndices m c < elemIndices n c

combinations :: Int -> [a] -> [[a]]
combinations = \ case
    0   -> const [[]]
    n+1 -> \ case 
        []   -> []
        x:xs -> map (x:) (combinations n xs) ++ combinations (n+1) xs
    _ -> error "negative"

toTuple :: [a] -> (a,a)
toTuple = \ case
    x:y:_ -> (x,y)
    _     -> error "toTuple: too short"



