module Types where

import           System.Random

-- Color

data Color = B -- Blue
           | G -- Green
           | Y -- Yellow
           | R -- Red
           | W -- White
           | K -- Black
  deriving (Bounded, Eq, Enum, Read, Show)

instance Random Color where
  randomR :: RandomGen g => (Color, Color) -> g -> (Color, g)
  randomR (a, b) g = case randomR (fromEnum a, fromEnum b) g
                       of (x, g') -> (toEnum x, g')
  random :: RandomGen g => g -> (Color, g)
  random = randomR (minBound, maxBound)

-- Code

data Guess
data Secret

newtype Code a = Code { unCode :: [Color] }
  deriving Show

-- Result

data NumPos
data NumColor

newtype Correct a = Correct Int
  deriving Show

type Result = ( Correct NumPos
              , Correct NumColor)

-- Game State

data GameEnv = GameEnv
  { unCodeLength :: Int
  , unNumGuesses :: Int
  } deriving Show

data GameState = GameState
  { unSecret  :: Code Secret
  , unGuesses :: [Code Guess]
  , unResults :: [Result] }
  deriving Show
