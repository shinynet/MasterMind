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
  g <- getStdGen -- newStdGen
  s <- get
  let pegList :: [Peg Color]
      pegList = Peg <$> colors g
      secret  :: Code Secret
      secret  = Code pegList
  put s { getSecret = secret }
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
    zipped :: [(Peg Color, Peg Color)]
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

printGameState :: GameState -> IO ()
printGameState gameState = go gameState 1 where
  go (GameState _ [] []) _ = TIO.putStrLn ""
  go s n = do
    let (g:gs) = getGuesses s
        (r:rs) = getResults s

    TIO.putStr $ T.pack $ "Guess " ++ show n ++ ": "
    printGuess g
    TIO.putStr " "
    printResult r

    let newS = GameState {
        getSecret  = getSecret s
      , getGuesses = gs
      , getResults = rs }

    go newS (n + 1)


printGuess :: Code Guess -> IO ()
printGuess (Code cs) = TIO.putStr $ T.pack pegs
  where pegs = concat $ (\(Peg c) -> show c) <$> cs


printResult :: Result -> IO ()
printResult (Correct p, Correct c) = do
  TIO.putStr $ T.pack $ show p
  TIO.putStr $ T.pack $ show c
  TIO.putStrLn ""
