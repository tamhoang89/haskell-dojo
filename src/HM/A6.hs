module HM.A6 where

import Data.Char (isAlpha)
import HM.Provided

-- Q#01
type Chances = Int
type Guess = String
type Move = Char
type Secret = String
type Dictionary = [String]

-- Q#02
data GameException =   InvalidChars
                     | InvalidLength
                     | NotInDict
                     | InvalidMove
                     | RepeatMove
                     | GameOver

-- Q#03
lengthInRange :: Secret -> Bool
lengthInRange s = let l = length s
                      (minL, maxL) = _LENGTH_
                  in  l >= minL && l <= maxL

-- Q#04
invalidMove :: Char -> Bool
invalidMove = not . isAlpha

-- Q#05
revealLetters :: Move -> Secret -> Guess -> Guess
revealLetters move sWord gWord = zipWith revealChar sWord gWord
    where revealChar sChar gChar = if sChar == move then sChar else gChar

-- Q#06
updateChances :: Move -> Secret -> Chances -> Chances
updateChances move gWord chances = if move `elem` gWord then chances else chances - 1

-- Q#07
setSecret :: IO String
setSecret = do
    putStr "Enter a secret word:\t"
    showInput False
    input <- getLine
    showInput True
    _SPACE_
    return input
    
