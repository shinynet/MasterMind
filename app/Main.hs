module Main where

import           Actions             (generateCode, getChar')
import qualified Data.Text.IO        as TIO
import           System.Console.ANSI (clearScreen, setCursorPosition)

main :: IO ()
main = do
  renderTitleScreen
  code <- generateCode
  peg1 <- getChar'
  peg2 <- getChar'
  peg3 <- getChar'
  peg4 <- getChar'
  print code

renderTitleScreen :: IO ()
renderTitleScreen = do
  TIO.putStrLn =<< TIO.readFile "title.txt"
  TIO.putStr "Press any key to continue "
  getChar
  clearScreen
  setCursorPosition 0 0
