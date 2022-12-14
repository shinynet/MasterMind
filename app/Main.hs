{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}
module Main where

import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import           Game
import           System.IO
import           Types


initialState :: GameState
initialState = GameState (Code []) [] []

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  renderTitleScreen
  e <- getEnvironment
  s <- execStateT (runReaderT generateSecret e) initialState
  void $ runReaderT printInstructions e
  -- uncomment to show secret code
  -- TIO.putStr "Secret: "
  -- printCode (unSecret state)
  void $ runStateT (runReaderT gameLoop e) s


gameLoop :: ReaderT GameEnv (StateT GameState IO) ()
gameLoop = do
  s <- get
  e <- ask

  let numGuesses = length $ unGuesses s
      maxGuesses = unNumGuesses e
      codeLength = unCodeLength e

  -- maximum tries reached
  if numGuesses == maxGuesses then do
    liftIO $ TIO.putStrLn "\nYou Lose"
    liftIO $ TIO.putStr "Secret Code: "
    liftIO $ printCode $ unSecret s
    liftIO $ TIO.putStrLn ""

  -- make guess
  else do
    liftIO $ TIO.putStr "\nNew Guess: "
    (guess, s') <- liftIO $ runStateT (runReaderT getGuess e) s

    let secret  = unSecret s
        guesses = unGuesses s
        (result, s'')  = runState getResult' s'

    liftIO $ runStateT (runReaderT printLine e) s''

    -- winning answer
    let (Correct numPos, _) = result
    if numPos == codeLength then do
      liftIO $ TIO.putStrLn "\nYou Win!"
    else do
      liftIO $ evalStateT (runReaderT gameLoop e) s''
