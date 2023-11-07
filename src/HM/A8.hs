module HM.A8 where

import Control.Monad (when)
import Data.Char (toUpper)
import HM.A6
import HM.A7 hiding (validateNoDict, validateWithDict)
import HM.Provided
import System.Directory (doesFileExist)

-- Q#01
getUpperChar :: IO Char
getUpperChar = toUpper <$> getChar

-- Q#02
_DICT_ :: IO Dictionary
_DICT_ = do
  fileExists <- doesFileExist _DICT_FILE_
  if fileExists then words <$> readFile _DICT_FILE_
                else pure []

isDictNonEmpty :: IO Bool
isDictNonEmpty = not . null <$> _DICT_

-- Q#03
makeGameIfValid :: Either GameException Secret -> Either GameException Game
makeGameIfValid checkingResult = makeGame <$> checkingResult

-- Q#04

getDict :: IO (Maybe Dictionary)
getDict = toMaybe <$> isDictNonEmpty <*> _DICT_

-- Q#05
validateNoDict :: Secret -> Either GameException Secret
validateNoDict s = hasValidChars s >>= isValidLength

validateWithDict :: Dictionary -> Secret -> Either GameException Secret
validateWithDict dic s = validateNoDict s >>= isInDict dic

-- Q#06
playGame :: Game -> IO ()
playGame game = do
  promptGuess
  move <- getUpperChar
  _SPACE_
  let sWord = getSecretWord game
  case processTurn move game of
    Left GameOver -> do print GameOver
                        putStrLn ("The correct word is: " ++ sWord)
    Left ex       -> do print ex
                        playGame game
    Right newGame -> do print newGame
                        let gWord = getGuessWord newGame
                        if gWord == sWord then putStrLn "Congratulation!!! You won."
                                          else playGame newGame

-- Q#07
startGame :: (Secret -> Either GameException Secret) -> IO ()
startGame checkSecretWord = do
  sWord <- setSecret
  case makeGameIfValid $ checkSecretWord sWord of
    Left ex    -> do print ex
                     startGame checkSecretWord
    Right game -> do print game
                     playGame game

-- Q#08

runHM :: IO ()
runHM = do
  maybeDict <- getDict
  case maybeDict of
    Just dict -> startGame (validateWithDict dict)
    Nothing   -> do
      putStrLn "Missing dictionary! Continue without dictionary? [Y/N]"
      contOrNot <- getUpperChar
      when (contOrNot == 'Y') (startGame validateNoDict)