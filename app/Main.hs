module Main where

import           Game
import           Utils

main :: IO ()
main = do
  renderTitleScreen
  code <- generateSecret
  peg1 <- getValidChar
  peg2 <- getValidChar
  peg3 <- getValidChar
  peg4 <- getValidChar
  print code


