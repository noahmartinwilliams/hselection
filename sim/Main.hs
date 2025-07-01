module Main (main) where

import System.Environment
import System.IO
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
import Parse
import Text.Megaparsec

go :: Double -> Int -> Int -> World -> IO ()
go maxSpeed cols rows world = do
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
            go maxSpeed cols rows world'
    else do
        end
        return ()
        
main :: IO ()
main = do
    args <- getArgs
    if (length args) == 0
    then do
        putStrLn ("Usage: sim [world filename]")
    else do
        let fname = args !! 0
        h <- openFile fname ReadMode
        c <- hGetContents h
        --let (rows, cols) = (25, 128)
        let dworld = runParser file "" c
        case dworld of 
            (Prelude.Left err) -> putStr (errorBundlePretty err)
            (Prelude.Right dworld') -> do
                start
                (rows, cols) <- scrSize
                timeout 1
                go 10.0 (2 * (div cols 3)) rows dworld'
        --putStrLn (foldr (++) "" (map (\x -> (show x ) ++ "\n")  commands'))
