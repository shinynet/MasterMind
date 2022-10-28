module Game where

import           Control.Monad.State
import           Data.Char
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
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
generateSecret :: StateT GameState IO ()
generateSecret = do
  g <- newStdGen -- getStdGen
  s <- get
  let colors :: [Color]
      colors = mkColors g
      secret  = Code colors
  put s { unSecret = secret }
  where mkColors = take 4 . randomRs (B, K)


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
  return $ Code colors


-- TODO: utilize Reader/State
-- for referencing Secret Code
getResult :: Code Secret -> Code Guess -> Result
getResult (Code s) (Code g) =
  ( Correct $ length cp
  , Correct $ length cc )
  where
    zipped :: [(Color, Color)]
    zipped   = zip s g
    (cp, ip) = partitionEq zipped
    (rs, rg) = unzip ip
    cc       = intersect' rs rg


isCharColor :: Char -> Bool
isCharColor c = c `elem` colorChars where
  allColors :: [Color]
  allColors  = allValues
  colorChars = concat $ show <$> allColors

-- pretty printing

printLine :: StateT GameState IO ()
printLine = do
  liftIO resetLine
  state <- get
  let guesses = unGuesses state
      results = unResults state
      guess   = head guesses
      result  = head results
      count   = length guesses
      pad | count >= 10 = " "
          | otherwise = "  "
  liftIO $ TIO.putStr $ T.pack $ pad ++ show count ++ ": "
  liftIO $ printCode guess
  liftIO $ printResult result


printCode :: Code a -> IO ()
printCode (Code colors) = TIO.putStr $ T.pack chars
  where chars = concat $ show <$> colors


printResult :: Result -> IO ()
printResult (Correct p, Correct c) = do
  TIO.putStr   $ T.pack $ replicate 3 ' ' ++ show p
  TIO.putStrLn $ T.pack $ replicate 3 ' ' ++ show c


printInstructions :: IO ()
printInstructions = do
  TIO.putStrLn "You have 10 tries to break the secret code."
  TIO.putStrLn "The secret code consists of 4 colors:"
  TIO.putStrLn "B (Blue), G (Green), Y (Yellow), R (Red),"
  TIO.putStrLn "W (White), and K (Black)"
  TIO.putStrLn ""
  TIO.putStrLn "Good Luck!"
  TIO.putStrLn ""
  TIO.putStrLn "P = Correct Color in Correct Position"
  TIO.putStrLn "C = Correct Color in Incorrect Position"
  TIO.putStrLn "---------------------------------------"
  TIO.putStrLn $ T.pack $ mconcat $ replicate 4 ' ' : 
    ["Guess | ", "P | ", "C"]

