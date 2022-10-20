module Main where

import           Actions (generateSecretCode, getInput, putTitleScreen)

main :: IO ()
main = do
  -- putTitleScreen
  secretCode <- generateSecretCode
  i1 <- getInput
  i2 <- getInput
  i3 <- getInput
  return ()
  -- print secretCode

