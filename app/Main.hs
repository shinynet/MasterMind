module Main where

import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import           Game
import           Types

initialState :: GameState
initialState = GameState (Code []) [] []

env :: GameEnv
env = GameEnv 4 10

main :: IO ()
main = do
  state  <- execStateT (runReaderT generateSecret env) initialState
  renderTitleScreen
  printInstructions
  -- uncomment to show secret code
  -- TIO.putStr "Secret: "
  -- printCode (unSecret state)
  void $ runStateT (runReaderT gameLoop env) state


gameLoop :: ReaderT GameEnv (StateT GameState IO) ()
gameLoop = do
  s <- get
  e <- ask

  let numGuesses = length $ unGuesses s

  -- maximum tries reached
  if numGuesses == 10 then do
    liftIO $ TIO.putStrLn "\nYou Lose"
    liftIO $ TIO.putStr "Secret Code: "
    liftIO $ printCode $ unSecret s
    liftIO $ TIO.putStrLn ""

  -- make guess
  else do
    liftIO $ TIO.putStr "\nNew Guess: "
    guess <- liftIO $ evalStateT (runReaderT getGuess e) s

    let secret  = unSecret s
        guesses = unGuesses s
        result  = getResult secret guess
    put s { unGuesses = guess  : unGuesses s
          , unResults = result : unResults s }
    printLine

    -- winning answer
    let (Correct numPos, _) = result
    if numPos == 4 then do
      liftIO $ TIO.putStrLn "\nYou Win!"
    else gameLoop
