module Actions (putTitleScreen, generateSecretCode) where

import qualified System.Process as SP
import           System.Random  (Random (randomRs), newStdGen)
import           Types          (CodePeg (CodePegB, CodePegK), Secret (..))

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

generateSecretCode :: IO Secret
generateSecretCode = do
  g <- newStdGen
  let pegs = take 4 (randomRs (CodePegB, CodePegK) g)
  return $ Secret pegs
