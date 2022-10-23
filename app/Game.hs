module Game where

import           Data.Char
import           Data.List
import qualified Data.Text.IO  as TIO
import           System.Random
import           Types
import           Utils

renderTitleScreen :: IO ()
renderTitleScreen = do
  TIO.putStrLn =<< TIO.readFile "title.txt"
  TIO.putStr "Press any key to continue "
  getChar
  resetScreen

-- TODO: utilize Reader/State
-- for random list length
generateSecret :: IO (Code Secret)
generateSecret = do
  g <- getStdGen -- newStdGen
  let pegList = Peg <$> colors g
      secret  = Code pegList
  return secret
  where colors = take 4 . randomRs (B, K)

-- TODO: utilize Reader/State
-- for Guess list length
getGuess :: IO (Code Guess)
getGuess = do
  char1 <- getValidChar toUpper isCharColor
  char2 <- getValidChar toUpper isCharColor
  char3 <- getValidChar toUpper isCharColor
  char4 <- getValidChar toUpper isCharColor
  let chars  = [char1, char2, char3, char4]
      colors = read . pure <$> chars
      pegs   = Peg <$> colors
  return $ Code pegs

-- TODO: utilize Reader/State
-- for referencing Secret Code
getResult :: Code Secret -> Code Guess -> Result
getResult (Code s) (Code g) =
  ( Correct $ length cp
  , Correct $ length cc )
  where
    zipped   = zip s g
    (cp, ip) = partitionEq zipped
    (rs, rg) = unzip ip
    cc       = intersect' rs rg

isCharColor :: Char -> Bool
isCharColor c = c `elem` colorChars where
  allColors :: [Color]
  allColors  = allValues
  colorChars = concat $ show <$> allColors
