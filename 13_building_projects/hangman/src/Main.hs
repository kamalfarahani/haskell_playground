module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

data Puzzle =
  Puzzle String [Maybe Char] [Char]

newtype WordList = 
  WordList [String]
  deriving (Eq, Show)

instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    (intersperse ' ' $ fmap renderPuzzleChar discovered)
    ++ " Guessed so far: " ++ (intersperse ',' guessed)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

maxIncorrectGuesses :: Int
maxIncorrectGuesses = 5

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)

gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList (filter gameLength aw)
  where
    gameLength word =
      let l = length word in
        l >= minWordLength && l < maxWordLength 

randomWord :: WordList -> IO String
randomWord (WordList wordList) = do
  randomIndex <- randomRIO (0, (length wordList) - 1)
  return (wordList !! randomIndex)

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

freshPuzzle :: String -> Puzzle
freshPuzzle word =
  Puzzle word nothings []
  where
    nothings = map (const Nothing) word

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _) ch = elem ch word

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) ch = elem ch guessed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar (Just ch) = ch
renderPuzzleChar Nothing = '_'

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar guessedChars) ch =
  Puzzle word newFilledInSoFar (ch:guessedChars)
  where
    newFilledInSoFar = zipWith (zipper ch) word filledInSoFar
    zipper guess wordChar maybeChar =
      if guess == wordChar
        then Just guess
        else maybeChar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "Already guessed that!"
      return puzzle
    (True, _) -> do
      putStrLn "You gussed a new char"
      return $ fillInCharacter puzzle guess
    (False, _) -> do
      putStrLn "Wrong guess!"
      return $ fillInCharacter puzzle guess

gameOver :: Puzzle -> IO ()
gameOver (Puzzle word _ guessed) =
  if incorrectGuesses > maxIncorrectGuesses 
    then
      do
        putStrLn "You lose!"
        putStrLn $ "The word was: " ++ word
        exitSuccess
    else
      return ()
  where
    incorrectGuesses = length $ filter notWordChar guessed
    notWordChar ch = not $ elem ch word 

gameWin :: Puzzle -> IO ()
gameWin (Puzzle word filledInSoFar _) =
  if all isJust filledInSoFar then
    do
      putStrLn "You won"
      putStr $ "The word was: " ++ word
      exitSuccess
  else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  putStrLn "\n_______________\n"
  gameOver puzzle
  gameWin puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _ -> putStrLn "Bad Input"

main :: IO ()
main = do
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle
