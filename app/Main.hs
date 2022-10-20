module Main where

import           Actions (generateSecretCode, putTitleScreen)

main :: IO ()
main = do
  -- putTitleScreen
  secretCode <- generateSecretCode
  print secretCode

