module Main where

import Memo
import Data.Word

{-
  fibonacci sequence
-}
fix :: (a -> a) -> a
fix f = let x = f x in x

fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibMemo :: Word8 -> Integer
fibMemo = memoize fib where
    fib 0 = 0
    fib 1 = 1
    fib n = fibMemo (n - 1) + fibMemo (n - 2)


{-
  knapsack
-}

type Weight = Word8
type Value = Double
type Item = (Weight, Value)

knapsack bound [] = 0
knapsack bound ((w, v):items) 
  | w > bound = knapsack bound items 
  | otherwise = max (knapsack bound items) (knapsack (bound - w) items + v)

knapsackMemo :: Weight -> [(Weight, Value)] -> Value
knapsackMemo w items = knapsackM w (length items - 1) where
  knapsackM = memoize2 knapsack

  knapsack bound (-1) = 0
  knapsack bound i
    | w > bound = knapsackM bound (i-1)
    | otherwise = max (knapsackM bound (i-1)) (knapsackM (bound - w) (i-1) + v)
    where
      ~(w, v) = items !! i

items :: [(Weight, Value)]
items = [(2, 6.0), (2, 3.0), (6, 5.0), (5, 4.0), (4, 6.0)
        ,(2, 6.0), (2, 3.0), (6, 5.0), (5, 4.0), (4, 6.0)
        ,(2, 6.0), (2, 3.0), (6, 5.0), (5, 4.0), (4, 6.0)
        ,(2, 6.0), (2, 3.0), (6, 5.0), (5, 4.0), (4, 6.0)
        ,(2, 6.0), (2, 3.0), (6, 5.0), (5, 4.0), (4, 6.0)
        ,(2, 6.0), (2, 3.0), (6, 5.0), (5, 4.0), (4, 6.0)
        ,(2, 6.0), (2, 3.0), (6, 5.0), (5, 4.0), (4, 6.0)]


main :: IO ()
main = mapM_ (print . fibMemo) [0..100]
