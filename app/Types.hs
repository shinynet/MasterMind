module Types where

import           System.Random (Random (random, randomR), RandomGen)

-- Player Pieces

data CodePeg = CodePegB -- Blue
             | CodePegG -- Green
             | CodePegY -- Yellow
             | CodePegR -- Red
             | CodePegW -- White
             | CodePegK -- Black
  deriving (Show, Enum, Bounded)

data KeyPeg = KeyPegW -- White
            | KeyPegK -- Black
  deriving Show

-- Game Types

newtype Secret = Secret [CodePeg] deriving Show

newtype Attemp = Attemp [CodePeg] deriving Show

newtype Result  = Result [KeyPeg] deriving Show

-- State

type GameState = (Secret, [(Attemp, Result)])

-- Instances

instance Random CodePeg where
  randomR :: RandomGen g => (CodePeg, CodePeg)
                         -> g
                         -> (CodePeg, g)
  randomR (a, b) g = case randomR (fromEnum a, fromEnum b) g of
                          (x, g') -> (toEnum x, g')
  random :: RandomGen g => g -> (CodePeg, g)
  random = randomR (minBound, maxBound)
