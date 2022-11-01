module Game where

import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Char
import           Data.Maybe
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import qualified System.Console.ANSI  as ANSI
import           System.Random
import           Text.Read            hiding (get)
import           Types
import           Utils


renderTitleScreen :: IO ()
renderTitleScreen = do
  resetScreen
  Just (_, width) <- ANSI.getTerminalSize
  if width >= 80
    then TIO.putStrLn =<< TIO.readFile "title.txt"
    else do
      ANSI.setSGR
        [ ANSI.SetConsoleIntensity ANSI.BoldIntensity
        , ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Blue ]
      TIO.putStr "Master"
      ANSI.setSGR
        [ ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Red ]
      TIO.putStrLn "Mind\n"
      ANSI.setSGR []


getEnvironment :: IO GameEnv
getEnvironment = do
  putStrLn "Set Code Length (default 4)"
  codeLengthInput <- getLine
  putStrLn "Set Number of Tries (default 10)"
  numGuessesInput <- getLine

  let codeLength = Data.Maybe.fromMaybe 4
        (readMaybe codeLengthInput :: Maybe Int)
      numGuesses = Data.Maybe.fromMaybe 10
        (readMaybe numGuessesInput :: Maybe Int)

  resetScreen
  return $ GameEnv codeLength numGuesses


generateSecret :: ReaderT GameEnv (StateT GameState IO) ()
generateSecret = do
  g <- newStdGen -- getStdGen
  s <- get
  e <- ask
  let codeLen = unCodeLength e
      colors  = mkColors g codeLen
      secret  = Code colors
  put s { unSecret = secret }
  where mkColors g codeLen = take codeLen $ randomRs (B, K) g


getGuess :: ReaderT GameEnv (StateT GameState IO) (Code Guess)
getGuess = do
  s <- get
  e <- ask
  let codeLen = unCodeLength e
      guesses = unGuesses s

  chars <- replicateM codeLen (liftIO $ getValidChar toUpper isCharColor)
  let code = Code $ read . pure <$> chars
  put s { unGuesses = code:guesses }

  return code


getResult' :: State GameState Result
getResult' = do
  s <- get

  let (Code secret)    = unSecret s
      ((Code guess):_) = unGuesses s
      results          = unResults s

  let zipped   = zip secret guess
      (cp, ip) = partitionEq zipped
      (rs, rg) = unzip ip
      cc       = intersect' rs rg

  let result = ( Correct $ length cp
               , Correct $ length cc )

  put s { unResults = result:results }
  return result


isCharColor :: Char -> Bool
isCharColor c = c `elem` colorChars where
  allColors :: [Color]
  allColors  = allValues
  colorChars = concat $ show <$> allColors


-- pretty printing

printLine :: ReaderT GameEnv (StateT GameState IO) ()
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
printCode (Code []) = ANSI.setSGR []
printCode (Code (c:cs)) = do
  let color = case c of
        B -> ANSI.Blue
        G -> ANSI.Green
        Y -> ANSI.Yellow
        R -> ANSI.Red
        W -> ANSI.White
        K -> ANSI.Black
  ANSI.setSGR
    [ ANSI.SetConsoleIntensity ANSI.BoldIntensity
    , ANSI.SetColor ANSI.Foreground ANSI.Vivid color
    ]
  TIO.putStr $ T.pack $ " " ++ show c ++ " "
  printCode $ Code cs


printResult :: Result -> IO ()
printResult (Correct p, Correct c) = do
  TIO.putStr   $ T.pack $ replicate 3 ' ' ++ show p
  TIO.putStrLn $ T.pack $ replicate 3 ' ' ++ show c


printInstructions :: ReaderT GameEnv IO ()
printInstructions = do
  GameEnv codeLength numGuesses <- ask

  putString $ T.pack $
    "You have " ++ show numGuesses ++ " tries to break the secret code."
  putString $ T.pack $
    "The secret code consists of " ++ show codeLength ++ " colors:"
  putString "B (Blue), G (Green), Y (Yellow), R (Red),"
  putString "W (White), and K (Black)"
  putString ""
  putString "Good Luck!"
  putString ""
  putString "P = Colors in Correct Position"
  putString "C = Colors in Incorrect Position"
  putString "--------------------------------"
  putString $ T.pack $ mconcat
    [ replicate 6 ' '
    , "Guess"
    , replicate 9 ' ' -- TODO: calculate based on code length
    , "P"
    , replicate 3 ' '
    , "C" ]
  where putString str = liftIO $ TIO.putStrLn str
