{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use print" #-}
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
  { getSecret = Code []
  , getGuesses = []
  , getResults = [] }

main :: IO ()
main = do
  -- renderTitleScreen
  state  <- execStateT generateSecret defaultState

  let secret = getSecret state
  TIO.putStr "Secret"
  TIO.putStrLn $ T.pack $ show secret

  result <- execStateT gameLoop state
  exitSuccess


gameLoop :: StateT GameState IO ()
gameLoop = do
  state <- get

  let numGuesses = length $ getGuesses state
  if numGuesses == 10 then do
    liftIO $ TIO.putStrLn "You Lose"
    -- return () -- not exiting game loop!!
    liftIO exitSuccess
  else liftIO $ TIO.putStr "New Guess: "

  guess <- liftIO getGuess
  let secret  = getSecret state
      guesses = getGuesses state
      result  = getResult secret guess
  put state
    { getGuesses = getGuesses state <> [guess]
    , getResults = getResults state <> [result] }

  statePrint <- get
  liftIO resetScreen
  liftIO $ printGameState statePrint


  let (Correct numPos, _) = result
  if numPos == 4 then do
    liftIO $ TIO.putStrLn "You Win!"
    return ()
  else gameLoop
