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

-- Peg

newtype Peg a = Peg a deriving (Eq, Show)

instance Functor Peg where
  fmap :: (a -> b) -> Peg a -> Peg b
  fmap f (Peg c) = Peg $ f c

instance Applicative Peg where
  pure :: a -> Peg a
  pure = Peg
  (<*>) :: Peg (a -> b) -> Peg a -> Peg b
  (<*>) (Peg f) = fmap f

instance Monad Peg where
  (>>=) :: Peg a -> (a -> Peg b) -> Peg b
  (>>=) (Peg x) f = f x

-- Code

data Guess
data Secret

newtype Code a = Code { unCode :: [Peg Color] }
  deriving Show

-- Result

data NumPos
data NumColor

newtype Correct a = Correct Int
  deriving Show

type Result = ( Correct NumPos
              , Correct NumColor)

-- Game State

data GameState = GameState
  { getSecret  :: Code Secret
  , getGuesses :: [Code Guess] 
  , getResults :: [Result] }
  deriving Show
