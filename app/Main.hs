module Main where

import           Control.Monad.State
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
import           Game
import           System.Exit
import           Types
import           Utils

defaultState :: GameState
defaultState = GameState
  { unSecret = Code []
  , unGuesses = []
  , unResults = [] }

main :: IO ()
main = do
  renderTitleScreen
  state  <- execStateT generateSecret defaultState

  -- uncomment to show secret code
  -- let secret = unSecret state
  -- TIO.putStr "Secret"
  -- TIO.putStrLn $ T.pack $ show secret
  printInstructions

  void $ execStateT gameLoop state


gameLoop :: StateT GameState IO ()
gameLoop = do
  state <- get

  let numGuesses = length $ unGuesses state
  if numGuesses == 10 then do
    liftIO $ TIO.putStrLn "You Lose"
    liftIO $ TIO.putStr "Secret Code: "
    liftIO $ printCode $ unSecret state
    liftIO $ TIO.putStrLn ""
  else do
    liftIO $ TIO.putStr "New Guess: "
    guess <- liftIO getGuess
    let secret  = unSecret state
        guesses = unGuesses state
        result  = getResult secret guess
    put state
      { unGuesses = unGuesses state <> [guess]
      , unResults = unResults state <> [result] }

    statePrint <- get
    liftIO resetScreen
    liftIO printInstructions
    liftIO $ printGameState statePrint

    let (Correct numPos, _) = result
    if numPos == 4 then do
      liftIO $ TIO.putStrLn "You Win!"
    else gameLoop
