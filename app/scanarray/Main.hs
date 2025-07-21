{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NPlusKPatterns #-}
module Main where

import Data.Array
import Data.List
import System.Environment
import Text.Printf
import Shuffled

main :: IO ()
main = print . minPath0 (gen 100) . read . (!! 0) =<< getArgs

minPath0 :: Array (Int,Int) Int -> (Int,Int) -> Int
minPath0 sa (1,1) = sa ! (1,1)
minPath0 sa (1,j) = minPath0 sa (1,j-1) + sa ! (1,j)
minPath0 sa (i,1) = minPath0 sa (i-1,1) + sa ! (i,1)
minPath0 sa (i,j) = minPath0 sa (i-1,j) `min` minPath0 sa (i,j-1) + sa ! (i,j)

sample :: Array (Int,Int) Int
sample = listArray ((1,1),(10,10)) (concat bss) where
    bss = [[8,9,6,10,5,4,2,7,3,1]
          ,[1,8,9,5,2,4,3,6,7,10]
          ,[3,7,2,5,4,8,6,10,1,9]
          ,[5,1,7,6,9,4,3,8,2,10]
          ,[6,9,2,3,1,8,10,4,7,5]
          ,[10,5,8,3,1,9,2,7,4,6]
          ,[3,7,9,6,2,4,1,8,10,5]
          ,[9,3,2,1,8,7,6,5,4,10]
          ,[6,9,2,1,8,10,5,7,4,3]
          ,[2,6,3,10,7,1,4,9,8,5]]

gen :: Int -> Array (Int,Int) Int
gen d = listArray ((1,1),(d,d)) (uncurry (+) <$> range ((1,1),(d,d)))

splitEvery :: Int -> [a] -> [[a]]
splitEvery n = unfoldr phi where
    phi = \ case
        [] -> Nothing
        xs -> Just (splitAt n xs)

getSample :: Int -> IO (Array (Int,Int) Int)
getSample n = listArray ((1,1),(n,n)) . concat <$> shuffled n

minPath0bis:: Array (Int,Int) Int -> (Int,Int) -> (Int, [(Int,Int)])
minPath0bis sa = \ case
    (1,1) -> (sa ! (1,1), [(1,1)])
    (1,j) -> case minPath0bis sa (1,j-1) of
        (p, ps) -> (p + sa ! (1,j), (1,j) : ps)
    (i,1) -> case minPath0bis sa (i-1,1) of
        (q, qs) -> (q + sa ! (i,1), (i,1) : qs)
    (i,j) -> case (minPath0bis sa (i,j-1), minPath0bis sa (i-1,j)) of
        ((p,ps),(q,qs))
            | p <= q    -> (p + sa ! (i,j), (i,j) : ps)
            | otherwise -> (q + sa ! (i,j), (i,j) : qs)
