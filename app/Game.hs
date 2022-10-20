module Game where

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

generateSecret :: IO Secret
generateSecret = do
  g <- getStdGen
  return $ Secret $ CodePeg <$> xs g
  where xs = take 4 . randomRs (B, K)
