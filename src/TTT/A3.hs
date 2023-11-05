module TTT.A3 where

import Data.List (transpose)
import TTT.A1
import TTT.A2

-- Q#01
showInts :: [Int] -> [String]
showInts (n:ns) = show n : showInts ns
showInts []     = []

_HEADER_ :: String
_HEADER_ = ' ' : formatLine (showInts _RANGE_)

-- Q#02
showSquares :: [Square] -> [String]
showSquares []       = []
showSquares (sq:sqs) = showSquare sq : showSquares sqs

-- Q#03
formatRows :: [Row] -> [String]
formatRows []     = []
formatRows (r:rs) = formatLine (showSquares r) : formatRows rs

-- Q#04
isColEmpty :: Row -> Int -> Bool
isColEmpty [] _     = False
isColEmpty (sq:sqs) n 
    | n < 0     = False
    | n == 0    = sq == E
    | otherwise = isColEmpty sqs (n-1)

-- Q#05
dropFirstCol :: Board -> Board
dropFirstCol [] = []
dropFirstCol (row:rows) = tail row : dropFirstCol rows

dropLastCol :: Board -> Board
dropLastCol [] = []
dropLastCol (row:rows) = init row : dropLastCol rows

-- Q#06
getDiag1 :: Board -> Line
getDiag1 []         = []
getDiag1 (row:rows) = head row : getDiag1 (dropFirstCol rows)

getDiag2 :: Board -> Line
getDiag2 []         = []
getDiag2 (row:rows) = last row : getDiag2 (dropLastCol rows)

getAllLines :: Board -> [Line]
getAllLines board = concat [
                        board,
                        transpose board,
                        [getDiag1 board, getDiag2 board]
                    ]

-- Q#07
putSquare :: Player -> Board -> Move -> Board
putSquare _ [] _              = []
putSquare p (row:rows) (x, y)
    | x > 0     = row : putSquare p rows (x-1, y)
    | x == 0    = replaceSquareInRow p y row : rows
    | otherwise = row : rows

-- Q#08
prependRowIndices :: [String] -> [String]
prependRowIndices strs = go (indexRowStrings strs)
    where go :: [(Char, String)] -> [String]
          go []                = []
          go ((ind, row) : rest) = (ind : row) : go rest

-- Q#09
isWinningLine :: Player -> Line -> Bool
isWinningLine p ln = go False ln
    where
        go :: Bool -> Line -> Bool 
        go res []       = res
        go _   (sq:sqs) = sq == p && go True sqs

-- Q#10
isValidMove :: Board -> Move -> Bool
isValidMove []    _  = False
isValidMove board mv = isMoveInBounds mv && go board mv
    where go :: Board -> Move -> Bool
          go (row:_)  (0, y) = isColEmpty row y
          go (_:rows) (x, y) = go rows (x-1, y)