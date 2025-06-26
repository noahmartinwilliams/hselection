module Main where

import HSelect.Types
import System.IO
import System.Random
import System.Environment
import Write
import HSelect.Gene
import HSelect.Spider
import HSelect.Plant

main :: IO ()
main = do
    g <- getStdGen
    args <- getArgs
    hSetBuffering stdout (BlockBuffering Nothing)
    let randInts = randoms g :: [Int]
        cols = read (args !! 0) :: Int
        rows = read (args !! 1) :: Int
        spiders = read (args !! 2) :: Int
        bugs = read (args !! 3) :: Int
        plants = read (args !! 4) :: Int
        world = genWorld randInts cols rows spiders bugs plants
    putStr (world2str world)

genWorld :: [Int] -> Int -> Int -> Int -> Int -> Int -> World
genWorld randInts cols rows numSpiders numBugs numPlants = do
    let (bugs, randInts') = randBugs randInts (2 * (div cols 3)) rows numBugs
        (spiders, randInts'') = randSpiders randInts' (2 * (div cols 3)) rows numSpiders
        (plants, randInts3) = randPlants randInts'' (2 * (div cols 3)) rows numPlants
    World cols (rows - 2) randInts3 spiders plants bugs []
