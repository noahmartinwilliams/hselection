module HSelect.Spider where

import HSelect.Types
randSpiders :: [Int] -> Int -> Int -> Int -> ([Spider], [Int])
randSpiders randLs cols rows n = do
    let xs = take n randLs
        ys = take n (drop n randLs)
        xy = zip xs ys
    (map (\(x, y) -> Spider ((mod (abs x) cols), (mod (abs y) rows)) 50) xy, (drop (2 * n) randLs))
