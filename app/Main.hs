module Main (main) where

import UI.HSCurses.CursesHelper
import UI.HSCurses.Curses
import Output
import System.Random
import Run
import Control.Monad.State
import Control.Monad.Writer
import World

main :: IO ()
main = do
    start
    (rows, cols) <- scrSize
    gi <- getStdGen
    let gd = mkStdGen 100
        randInts = randoms gi :: [Int]
        randDoubles = randoms gd :: [Double]
        dworld = defaultWorld randInts randDoubles ( 2 * (div cols 3)) rows
        runner = run randInts randDoubles cols (rows - 2) 3.0
        written = runStateT runner dworld
        ((commands, _), _) = runWriter written
    obey stdScr commands cols rows 
    end
