module HM.A7 where

import Data.Char (isAlpha, toLower, toUpper)
import HM.A6
import HM.Provided
import System.Directory (doesFileExist)
import Data.List (intersperse, sort)

-- Q#01
data Game = Game { getSecretWord :: Secret
                 , getGuessWord  :: Guess
                 , getMoves      :: [Move]
                 , getChances    :: Chances
                 }

-- Q#02
repeatedMove :: Move -> Game -> Bool
repeatedMove move game = move `elem` getMoves game

-- Q#03
makeGame :: Secret -> Game
makeGame sWord = 
  Game {
    getSecretWord = map toUpper sWord,
    getGuessWord  = map (const '_') sWord,
    getMoves      = [],
    getChances    = _CHANCES_
  }

-- Q#04
updateGame :: Move -> Game -> Game
updateGame move game = game {
    getGuessWord = revealLetters move sWord gWord,
    getMoves     = move : moves,
    getChances   = updateChances move sWord chances
  }
  where
    Game sWord gWord moves chances = game

-- Q#05
instance Show Game where
  show :: Game -> String
  show (Game _ gWord moves chances) = showGameHelper gWord moves chances 
  
showGameHelper :: String -> [Char] -> Int -> String
showGameHelper gWord moves chances =
  unlines
    [ _STARS_,
      "\tSecret Word:\t" ++ intersperse ' ' gWord ++ "\n",
      "\tGuessed:\t" ++ intersperse ' ' (sort moves) ++ "\n",
      "\tChances:\t" ++ show chances,
      _STARS_
    ]

-- Q#06
instance Show GameException where
    show :: GameException -> String
    show ex = case ex of
        InvalidChars  -> "Invalid Character."
        InvalidLength -> "Invalid Length." ++ ilStr
        NotInDict     -> "Not In Dictionary."
        InvalidMove   -> "Invalid Move."
        RepeatMove    -> "Repeat Move."
        GameOver      -> "Game Over!!!"
        where lb = show $ fst _LENGTH_
              ub = show $ snd _LENGTH_
              ilStr = concat [ " The word must be "
                            , lb
                            , " to "
                            , ub
                            , " characters long." ]

-- Q#07
toMaybe :: Bool -> a -> Maybe a
toMaybe bool val = if bool then Just val else Nothing

-- Q#08
validateSecret :: (Secret -> Bool) -> GameException -> Secret -> Either GameException Secret
validateSecret p ex sWord = if p sWord then Right sWord
                                       else Left ex    

-- Q#09
hasValidChars :: Secret -> Either GameException Secret
hasValidChars = validateSecret (all isAlpha) InvalidChars

isValidLength :: Secret -> Either GameException Secret
isValidLength = validateSecret lengthInRange InvalidLength

isInDict :: Dictionary -> Secret -> Either GameException Secret
isInDict dict = validateSecret (`elem` dict) NotInDict

-- Q#10
validateNoDict :: Secret -> Either GameException Secret
validateNoDict sWord = case hasValidChars sWord of
    Right sWord' -> isValidLength sWord'
    Left msg     -> Left msg

validateWithDict :: Dictionary -> Secret -> Either GameException Secret
validateWithDict dict sWord = case validateNoDict sWord of
    Right sWord' -> isInDict dict sWord'
    Left msg     -> Left msg

-- Q#11
processTurn :: Move -> Game -> Either GameException Game
processTurn move game
    | invalidMove move       = Left InvalidChars
    | repeatedMove move game = Left RepeatMove
    | newChances == 0        = Left GameOver
    | otherwise              = Right newGame
    where
      newGame = updateGame move game
      newChances = getChances newGame