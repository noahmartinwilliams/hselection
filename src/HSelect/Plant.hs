module HSelect.Plant where

import HSelect.Types
import Data.List

randPlants :: [Int] -> Int -> Int -> Int -> ([Plant], [Int])
randPlants randLs cols rows n = do
    let xs = take n randLs
        ys = take n (drop n randLs)
        xy = zip xs ys
    (map (\(x, y) -> Plant ((mod (abs x) cols), (mod (abs y) rows)) 10) xy, drop (2 * n) randLs)
