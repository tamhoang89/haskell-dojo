module TTT.A5 where

import Control.Monad (when)
import System.Random.Stateful (globalStdGen, uniformM)
import TTT.A1
import TTT.A2
import TTT.A3
import TTT.A4

-- Q#01
printBoard :: Board -> IO ()
printBoard = putStrLn . formatBoard

-- Q#02
_LOGO_PATH_ :: FilePath
_LOGO_PATH_ = "./assets/ttt-logo.txt"

printLogo :: IO ()
printLogo = readFile _LOGO_PATH_ >>= putStrLn

-- Q#03
_RANDOM_BOOL_ :: IO Bool
_RANDOM_BOOL_ = uniformM globalStdGen

firstPlayer :: IO Player
firstPlayer = _RANDOM_BOOL_ >>= (\b -> return $ getFirstPlayer b)

-- Q#04
getMove :: Board -> IO Move
getMove board = getLine 
                >>= return . stringToMove
                >>= go 
    where
        go :: Move -> IO Move
        go mv = if isValidMove board mv
                    then print mv 
                         >> return mv
                    else putStrLn "Invalid move! Try again" 
                         >> getMove board

-- Q#05
play :: Board -> Player -> IO ()
play board player =
    when _DISPLAY_LOGO_ printLogo
    >> printBoard board
    >> putStrLn (promptPlayer player)
    >> getMove board >>= (\ mv ->
        (\ (newState, newBoard) ->
            if newState == InProgress
                then play newBoard $ switchPlayer player
                else printBoard newBoard
                     >> putStrLn (showGameState newState)
        ) (playMove player board mv) )
-- Q#06

runTTT :: IO ()
runTTT = firstPlayer >>= play _EMPTY_BOARD_

-- Q#07
printLogoDo :: IO ()
printLogoDo = do
    logo <- readFile _LOGO_PATH_
    putStrLn logo

-- Q#08
firstPlayerDo :: IO Player
firstPlayerDo = do
    randBool <- _RANDOM_BOOL_
    return $ getFirstPlayer randBool

-- Q#09
getMoveDo :: Board -> IO Move
getMoveDo board = do
    mvStr <- getLine
    let mv = stringToMove mvStr
    if isValidMove board mv
        then do
            print mv
            return mv
        else do
            putStrLn "Invalid move! Try again"
            getMove board

-- Q#10
playDo :: Board -> Player -> IO ()
playDo board player = do
    when _DISPLAY_LOGO_ printLogo
    printBoard board
    putStrLn $ promptPlayer player
    mv <- getMove board
    let (newState, newBoard) = playMove player board mv
    if newState == InProgress
        then play newBoard $ switchPlayer player
        else do
            printBoard newBoard
            putStrLn $ showGameState newState