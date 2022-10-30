module Playground where

import           Control.Monad.Reader
import           Control.Monad.State

-- ReaderT r m a = ReaderT { runReaderT :: r -> m a }
-- StateT s m a = StateT { runStateT :: s -> m (a, s) }

data GameState = GameState
  { guesses :: [String]
  , results :: [String]
  } deriving Show

initialGameState :: GameState
initialGameState = GameState [] []

data GameEnv = GameEnv
  { codeLength :: Int
  , numGuesses :: Int
  , secretCode :: [String]
  } deriving Show

initialGameEnv :: GameEnv
initialGameEnv = GameEnv 4 10 []

playground :: IO ()
playground = do
  void $ runStateT (runReaderT doStuff initialGameEnv) initialGameState

doStuff :: ReaderT GameEnv (StateT GameState IO) ()
doStuff = do
  e <- ask
  s <- get
  liftIO $ print e
  liftIO $ print s
