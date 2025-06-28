module Main where

import HSelect.Types
import System.IO
import System.Random
import System.Environment
import Write
import HSelect.Gene
import HSelect.Spider
import HSelect.Plant
import System.Console.GetOpt

main :: IO ()
main = do
    g <- getStdGen
    args <- getArgs
    hSetBuffering stdout (BlockBuffering Nothing)
    let (actions, nonOptions, errors) = getOpt RequireOrder options args
    opts <- foldl (>>=) (return (Flags { needHelp = False, cols = 211, rows=26, numSpiders = 30, numPlants = 20, numBugs = 100})) actions
    if needHelp opts
    then
        putStrLn (usageInfo "gen: " options) 
    else do
        let randInts = randoms g :: [Int]
            numcols = cols opts
            numrows = rows opts
            spiders = numSpiders opts
            bugs = numBugs opts
            plants = numPlants opts
            world = genWorld randInts numcols numrows spiders bugs plants
        putStr (world2str world)

genWorld :: [Int] -> Int -> Int -> Int -> Int -> Int -> World
genWorld randInts cols rows numSpiders numBugs numPlants = do
    let (bugs, randInts') = randBugs randInts (2 * (div cols 3)) rows numBugs
        (spiders, randInts'') = randSpiders randInts' (2 * (div cols 3)) rows numSpiders
        (plants, randInts3) = randPlants randInts'' (2 * (div cols 3)) rows numPlants
    World cols (rows - 2) randInts3 spiders plants bugs []

data Flags = Flags { needHelp :: Bool, cols :: Int, rows :: Int, numSpiders :: Int, numBugs :: Int, numPlants :: Int } deriving(Show, Eq)

options :: [OptDescr (Flags -> IO Flags)]
options = [ Option ['r'] ["rows"] (ReqArg (\i -> \opt -> return opt { rows = (read i :: Int) }) "26") "Number of rows of the terminal.",
    Option ['c'] ["cols"] (ReqArg (\i -> \opt -> return opt { cols = (read i :: Int)}) "211") "Number of columns of the terminal.",
    Option ['b'] ["bugs"] (ReqArg (\i -> \opt -> return opt { numBugs = (read i :: Int)}) "100") "Number of bugs to populate world with.",
    Option ['s'] ["spiders"] (ReqArg (\i -> \opt -> return opt { numSpiders = (read i :: Int)}) "20") "Number of spiders.",
    Option ['p'] ["plants"] (ReqArg (\i -> \opt -> return opt { numPlants = (read i :: Int)}) "30") "Number of plants.",
    Option ['h'] ["help"] (NoArg (\opt -> return opt { needHelp = True})) "Display help."]

