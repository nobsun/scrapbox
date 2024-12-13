module Fib where

import Data.Array

fib :: Array Int Integer -> Int -> Integer
fib sa n = ta ! n 
    where
        ta = listArray (bounds sa) (phi <$> assocs sa)
        phi = \ case
            (0,j) -> j
            (1,j) -> j
            (i,_) -> ta ! (i-2) + ta ! (i-1)

nats :: Array Int Integer
nats = listArray (0,1000) [0 .. 1000]
