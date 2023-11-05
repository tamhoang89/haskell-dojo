module TTT.A4 where

import Data.List (transpose)
import TTT.A1
import TTT.A2
import TTT.A3 (getAllLines, putSquare)
import Text.ParserCombinators.ReadPrec (reset)

-- Q#01
_HEADER_ :: String
_HEADER_ = ' ' : formatLine (map show _RANGE_)

-- Q#02
showSquares :: [Square] -> [String]
showSquares = map showSquare

-- Q#03
dropFirstCol :: Board -> Board
dropFirstCol = map tail

-- Q#04
dropLastCol :: Board -> Board
dropLastCol = map init

--Q#05
formatRows :: [Row] -> [String]
formatRows = map (\row -> formatLine $ showSquares row)

-- Q#06
isWinningLine_ :: Player -> Line -> Bool
isWinningLine_ _ [] = False
isWinningLine_ p ln = null $ filter (/= p) ln

-- Q#07
isWinningLine :: Player -> Line -> Bool
isWinningLine _ [] = False
isWinningLine p ln = foldr (\r acc -> r == p && acc) True ln

-- Q#08
hasWon :: Player -> Board -> Bool
hasWon p board = foldr (\ln res -> isWinningLine p ln || res)
                       False
                       $ getAllLines board

_X_WIN_ = [ [X, O, O]
          , [O, X, O]
          , [O, O, X]
          ]

_O_WIN_ = [ [O, X, O]
          , [X, X, O]
          , [X, O, O]
          ]

-- Q#09
getGameState :: Board -> GameState
getGameState board
    | hasWon X board = WonX
    | hasWon O board = WonO
    | isTied board   = Tie
    | otherwise      = InProgress

playMove :: Player -> Board -> Move -> (GameState, Board)
playMove player board mv =
    let newBoard = putSquare player board mv
        newState = getGameState newBoard
    in  (newState, newBoard)

-- Q#10
prependRowIndices :: [String] -> [String]
prependRowIndices = zipWith (:) ['A'..]

-- Q#11
formatBoard :: Board -> String
formatBoard = unlines . (_HEADER_ :) . prependRowIndices . formatRows