module TTT.A2 where

import Data.List (intercalate)
import TTT.A1

-- Q#01
promptPlayer :: Player -> String
promptPlayer player = concat ["Player ", show player, "'s turn: enter a row and column position (ex. A1)"]

-- Q#02
_RANGE_ :: [Int]
_RANGE_ = [0..(_SIZE_ -1)]

-- Q#03
isDigit :: Char -> Bool
isDigit c = c `elem` ['0'..'9']

readDigit :: Char -> Int
readDigit c
    | isDigit c = read [c]
    | otherwise = -1

-- Q#04
_EMPTY_ROW_ :: Row
_EMPTY_ROW_ = replicate _SIZE_ E

_EMPTY_BOARD_ :: Board
_EMPTY_BOARD_ = replicate _SIZE_ _EMPTY_ROW_

-- Q#05
isTied :: Board -> Bool
isTied board = E `notElem` concat board

_TIED_BOARD_ :: Board
_TIED_BOARD_ = [
    [X, O, O]
  , [O, X, X]
  , [O, X, O]
  ]

-- Q#06
indexRowStrings :: [String] -> [(Char, String)]
indexRowStrings xs = zip ['A'..] xs

-- Q#07
formatLine :: [String] -> String
formatLine line = _SEP_ ++ intercalate _SEP_ line ++ _SEP_

-- Q#08
isMoveInBounds :: Move -> Bool
isMoveInBounds (x, y) = x >= 0      &&
                        y >= 0      &&
                        x <  _SIZE_ &&
                        y <  _SIZE_

-- Q#09
stringToMove :: String -> Move
stringToMove [x, y] = (convertRowIndex x, readDigit y)
stringToMove _      = _INVALID_MOVE_

-- Q#10
replaceSquareInRow :: Player -> Int -> Row -> Row
replaceSquareInRow player index row
    | index < 0 || index >= length row = row
    | otherwise = fstPart ++ (player : sndPart)
        where 
            (fstPart, _ : sndPart) = splitAt index row 

rsX = replaceSquareInRow X
rsO = replaceSquareInRow O