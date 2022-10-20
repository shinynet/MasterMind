module Types where

import           System.Random

data Color = B -- Blue
           | G -- Green
           | Y -- Yellow
           | R -- Red
           | W -- White
           | K -- Black
  deriving (Show, Enum, Bounded)

-- Game Types

newtype CodePeg = CodePeg Color deriving Show

newtype KeyPeg = KeyPeg Color deriving Show

newtype Secret = Secret [CodePeg] deriving Show

newtype Attemp = Attemp [CodePeg] deriving Show

newtype Result = Result [KeyPeg] deriving Show

-- State

type GameState = (Secret, [(Attemp, Result)])

-- Instances

instance Random Color where
  randomR :: RandomGen g => (Color, Color) -> g -> (Color, g)
  randomR (a, b) g = case randomR (fromEnum a, fromEnum b) g
                       of (x, g') -> (toEnum x, g')
  random :: RandomGen g => g -> (Color, g)
  random = randomR (minBound, maxBound)
