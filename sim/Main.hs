module Main (main) where

import UI.HSCurses.CursesHelper
import UI.HSCurses.Curses
import Output
import System.Random
import Run
import Control.Monad.State
import Control.Monad.Writer
import Control.Parallel.Strategies
import GHC.Conc
import World
import HSelect.Types

go :: Double -> [Int] -> Int -> Int -> World -> IO ()
go maxSpeed randInts cols rows world = do
    inp <- getch
    let key = decodeKey inp
    if inp == -1 
    then do
        let runner = run maxSpeed
            written = runStateT runner world
            ((commands, world'), _) = runWriter written
            (World _ _ _ _ _ bugLs _) = world'
        if (length bugLs) == 0
        then do
            end
            return()
        else do
            let commands' = commands `using` (parBuffer numCapabilities rdeepseq)
            obey stdScr commands' cols rows 
            go maxSpeed randInts cols rows world'
    else do
        end
        return ()
        


main :: IO ()
main = do
    start
    (rows, cols) <- scrSize
    timeout 1
    --let (rows, cols) = (25, 128)
    gi <- getStdGen
    let randInts = randoms gi :: [Int]
    let dworld = defaultWorld randInts ( 2 * (div cols 3)) rows -- use 2/3 to keep the creatures from crawling over the log on the right of the screen.
    go 10.0 randInts (2 * (div cols 3)) rows dworld
    --putStrLn (foldr (++) "" (map (\x -> (show x ) ++ "\n")  commands'))
