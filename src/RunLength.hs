{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE LambdaCase, MultiWayIf #-}
module RunLength where

import Control.Arrow
import Data.Functor.Foldable
import Data.List

type RLE a = [(a, Int)]

{- |
>>> let xs = "aaabcc"
>>> toRLE xs
[('a',3),('b',1),('c',2)]

-}
toRLE :: forall a. Eq a => [a] -> RLE a
toRLE = ana (para phi) where
    phi :: ListF a ([a], ListF (a,Int) [a]) -> ListF (a,Int) [a]
    phi = \ case
        Nil -> Nil
        Cons x (_, Nil) -> Cons (x,1) []
        Cons x (xs, Cons (y,m) ys)
            | x == y    -> Cons (x,succ m) ys
            | otherwise -> Cons (x,1) xs

-- spanCount :: (a -> Bool) -> [a] -> (Int, [a])
-- spanCount p = para phi
--     where
--         phi = \ case
--             Nil -> (0, [])
--             Cons x (xs, (z,zs))
--                 | p x       -> (succ z, zs)
--                 | otherwise -> (0, x:xs)
-- spanCount p = \ case
--     x:xs | p x -> case spanCount p xs of
--         (m,ys)     -> (succ m, ys)
--     xs         -> (0,xs)

{- |
>>> let xs = "aaabcc"
>>> fromRLE (toRLE xs) == xs
True
-}
fromRLE :: RLE a -> [a]
fromRLE = (uncurry (flip replicate) =<<)

rleSplitAt :: Int -> [(a, Int)] -> ([(a, Int)], [(a, Int)])
rleSplitAt = \ case
    n+1 -> \ case
        h@(x,m+1):rs
            | n < m     -> ([(x,n+1)],(x,m-n):rs)
            | otherwise -> case rleSplitAt (n-m) rs of
                (as,bs)     -> (h:as,bs)
        _               -> ([],[])
    _   -> ([],)

transposeRLE :: RLE Int -> RLE Int
transposeRLE rle = iter len rle where
    len    = sum (snd <$> rle)
    iter n = \ case
        []         -> []
        (a,m) : rs -> (n,a) : iter (n - m) (map (first (subtract a)) rs)

{- ^
>>> import Control.Arrow
>>> let xs = "aaabcc"
>>> let expected n = splitAt n xs == (fromRLE *** fromRLE $ rleSplitAt n (toRLE xs))
>>> all expected [0 .. length xs]
True

-}

type Query = [Int]
type Output = Int

solve :: [Query] -> [Output]
solve = ana psi . cata phi where
    phi = \ case
        Nil       -> ([],[])
        Cons a b -> case a of
            [1,c,x]   -> first ((x,c) :) b
            [2,k]     -> second (k :) b
            _         -> error "invalid query"
    psi = \ case
        (_,[])   -> Nil
        (a,k:ks) -> case rleSplitAt k a of
            (b,c)    -> Cons (foldl' (\ x (y,z) -> x + y * z) (0 :: Int) b) (c,ks)

{- ^
>>> let qs = [[1, 160449218, 954291757],[2, 17217760],[1,353195922,501899080],[1, 350034067, 910748511],[1, 824284691, 470338674],[2, 180999835],[1, 131381221, 677959980],[1, 346948152, 208032501],[1, 893229302, 506147731],[2, 298309896]]
>>> solve qs
[16430766442004320,155640513381884866,149721462357295680]

-}
