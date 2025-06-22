module Bug(Bug(..), randBugs, drawBug, obeyGenes) where

import Pos
import Commands
import Log
import Control.Monad.Writer

data Bug = Bug { bugPosn :: Pos, bugEnergy :: Int, bugGenes :: [Gene], bugCurrentGene :: Int } deriving(Show, Eq, Ord)

data Gene = Up | Down | Left | Right deriving(Show, Eq, Ord)

obeyGenes :: Int -> Int -> Bug -> Writer [LogEntry] Bug
obeyGenes cols rows bug@(Bug { bugCurrentGene = x, bugGenes = y}) | (x == (length y)) = obeyGenes cols rows (bug {bugCurrentGene = 0})
obeyGenes cols rows bug@(Bug { bugCurrentGene = i, bugEnergy = e, bugGenes = g, bugPosn = p@(x, y)}) = do
    case (g !! i) of
        Up -> 
            let (bounced, newPos) = adjustPos (x, y+1) cols rows in bugBounce bounced newPos bug
        Down ->
            let (bounced, newPos) = adjustPos (x, y-1) cols rows in bugBounce bounced newPos bug
        Bug.Left ->
            let (bounced, newPos) = adjustPos (x-1, y) cols rows in bugBounce bounced newPos bug
        Bug.Right ->
            let (bounced, newPos) = adjustPos (x+1, y) cols rows in bugBounce bounced newPos bug 

bugBounce :: Bool -> Pos -> Bug -> Writer [LogEntry] Bug
bugBounce bounced newPos bug@(Bug {bugPosn = p, bugEnergy = e, bugCurrentGene = currentGene}) = do
    let currentGene' = currentGene + 1
    if bounced 
    then do 
        tell [BugBounced p] ; 
        return (bug {bugEnergy = (e-1)} ) 
    else 
        return (bug{bugEnergy = (e - 1), bugPosn = newPos, bugCurrentGene = currentGene'})

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

