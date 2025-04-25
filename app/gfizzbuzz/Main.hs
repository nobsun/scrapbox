{-# LANGUAGE GHC2021 #-}
{- 一般化FizzBuzz -}
module Main where
import Data.Bool
main :: IO ()
main = putStr 
     $ unlines 
     $ gfizzbuzz [(3,"Fizz"),(5,"Buzz"),(7,"Hoge")] (101,140)
{- ^ 3の倍数 Fizz、5の倍数 Buzz、7の倍数 Hoge、範囲 101〜140 
>>> main
101
Fizz
103
104
FizzBuzzHoge
106
107
Fizz
109
Buzz
Fizz
Hoge
113
Fizz
Buzz
116
Fizz
118
Hoge
FizzBuzz
121
122
Fizz
124
Buzz
FizzHoge
127
128
Fizz
Buzz
131
Fizz
Hoge
134
FizzBuzz
136
137
Fizz
139
BuzzHoge
-}
gfizzbuzz :: [(Int,String)] -> (Int,Int) -> [String]
gfizzbuzz seeds (lo,hi) 
    = take (succ (hi - lo)) 
    $ drop lo
    $ zipWith (+>) (show <$> [0 :: Int ..])
    $ foldr1 (zipWith (++))
    $ map phi seeds
    where
        phi (i,s) = cycle (take i (s : repeat ""))

(+>) :: [a] -> [a] -> [a]
(+>) xs ys = bool ys xs (null ys)
