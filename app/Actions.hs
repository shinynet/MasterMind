module Actions (putTitleScreen, generateSecretCode, getInput) where

import           Data.Char      (toUpper)
import           System.IO
import qualified System.Process as SP
import           System.Random  (Random (randomRs), newStdGen)
import           Types          (Attemp, CodePeg (CodePegB, CodePegK),
                                 Secret (..))
import           Utils          (isValidInput)

generateSecretCode :: IO Secret
generateSecretCode = do
  g <- newStdGen
  let pegs = take 4 (randomRs (CodePegB, CodePegK) g)
  return $ Secret pegs

-- Prevents input and echoing of input for invalid input
getInput :: IO Char
getInput = do
  hSetEcho stdin False
  input <- getChar
  let upper = toUpper input
  hSetEcho stdin True
  if isValidInput upper
    then do
      putStr [upper]
      return upper
    else getInput

-- makeGuess :: IO Attempt
-- makeGuess = do
--   putStrLn "Take a guess..."
--   putStrLn "B (Blue), G (Green), Y (Yellow), R (Red), W (White), K (Black)"
--   c1 <- toUpper . getChar
--   c2 <- toUpper . getChar
--   c3 <- toUpper . getChar
--   c4 <- toUpper . getChar

putTitleScreen :: IO ()
putTitleScreen = do
  contents <- readFile "titleScreen.txt"
  putStrLn contents
  putStr "Press any key to continue"
  getLine
  clearScreen

clearScreen :: IO ()
clearScreen = do
  SP.system "reset"
  return ()


