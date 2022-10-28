module Main where

import           Control.Monad.State
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
import           Game
import           Types

defaultState :: GameState
defaultState = GameState
  { unSecret = Code []
  , unGuesses = []
  , unResults = [] }

main :: IO ()
main = do
  state  <- execStateT generateSecret defaultState
  renderTitleScreen
  printInstructions
  -- uncomment to show secret code
  -- TIO.putStr "Secret: "
  -- printCode (unSecret state)
  void $ execStateT gameLoop state


gameLoop :: StateT GameState IO ()
gameLoop = do
  state <- get

  let numGuesses = length $ unGuesses state

  -- maximum tries reached
  if numGuesses == 10 then do
    liftIO $ TIO.putStrLn "\nYou Lose"
    liftIO $ TIO.putStr "Secret Code: "
    liftIO $ printCode $ unSecret state
    liftIO $ TIO.putStrLn ""

  -- make guess
  else do
    liftIO $ TIO.putStr "\nNew Guess: "
    guess <- liftIO getGuess
    let secret  = unSecret state
        guesses = unGuesses state
        result  = getResult secret guess
    put state { unGuesses = guess  : unGuesses state
              , unResults = result : unResults state }
    printLine

    -- winning answer
    let (Correct numPos, _) = result
    if numPos == 4 then do
      liftIO $ TIO.putStrLn "\nYou Win!"
    else gameLoop
