module MinPath where

import Data.Array
import Data.List.Extra
import Shuffled
import ScanArray
{- | minPath0
    仕様 
>>> minPath sample (10,10)
-}
minPath0 :: Array (Int,Int) Int -> (Int,Int) -> Int
minPath0 sa = \ case
    (1,1) -> sa ! (1,1)
    (1,j) -> minPath0 sa (1,j-1) + sa ! (1,j)
    (i,1) -> minPath0 sa (i-1,1) + sa ! (i,1)
    (i,j) -> min (minPath0 sa (i,j-1)) (minPath0 sa (i-1,j)) + sa ! (i,j)

sample :: Array (Int,Int) Int
{- v1 --
sample = listArray ((1,1),(10,10)) [ i+j | i <- [1..10], j <- [1..10]]
-- -}
{- v2 -}
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
-- -}

genSample :: Int -> IO ()
genSample n = writeFile "sample.txt" 
            . (++ "\n")
            . show 
            . listArray ((1,1),(n,n)) . concat =<< shuffled n

getSample :: IO (Array (Int,Int) Int)
getSample = read <$> readFile "sample.txt"

{- -}
minPath :: Array (Int,Int) Int -> ((Int,Int) -> Int)
minPath sa = (ta !) where
    ta = listArray (bounds sa) (phi <$> assocs sa)
    phi = \ case
        ((1,1), s) -> s
        ((1,j), s) -> ta ! (1,j-1) + s
        ((i,1), s) -> ta ! (i-1,1) + s
        ((i,j), s) -> (ta ! (i,j-1)) `min` (ta ! (i-1,j)) + s
-- -}

{- -}
minPath2 :: Array (Int,Int) Int -> ((Int,Int) -> Int)
minPath2 = (!) . scanArray id (+) h where
    h _ a b c = a `min` b + c
