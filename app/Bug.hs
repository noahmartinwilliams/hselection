module Bug(Bug(..), randBugs, drawBug) where

import Pos
import Commands
import Log
import Control.Monad.Writer

data Bug = Bug { bugPosn :: Pos, bugEnergy :: Int, bugGenes :: [Gene], bugCurrentGene :: Int } deriving(Show, Eq, Ord)

data Gene = Up | Down | Left | Right deriving(Show, Eq, Ord)

randBug :: [Int] -> Int -> Int -> (Bug, [Int])
randBug (randX : randY : rest ) cols rows = do
    let randX' = mod (abs randX) cols
        randY' = mod (abs randY) rows
    (Bug { bugPosn = (randX', randY'), bugEnergy = 15, bugGenes = [Up, Down, Bug.Left, Bug.Right], bugCurrentGene = 0}, rest)

randBugs :: [Int] -> Int -> Int -> Int -> ([Bug], [Int])
randBugs rest _ _ 0 = ([], rest)
randBugs randLs cols rows numBugs = do
    let (bug, randLs') = randBug randLs cols rows
        (bugs, randLs'') = randBugs randLs' cols rows (numBugs - 1)
    (bug : bugs, randLs'')

drawBug :: Int -> Int -> Bug -> Writer [LogEntry] [Command]
drawBug cols rows bug@(Bug {bugPosn = posn}) = do
    let (bounced, posn') = adjustPos posn cols rows
    if bounced
    then do
        tell [BugBounced posn]
        return [DrawBug posn]
    else
        return [DrawBug posn']
