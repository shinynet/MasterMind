module Foo (greetUser, getLuckyNumber) where

import           System.Random (randomRIO)

getLuckyNumber :: IO Int
getLuckyNumber = randomRIO (1, 100)

greetUser :: IO [Char]
greetUser = do
  name <- getLine
  pure $ "Hello " ++ name ++ "!"

-- main :: IO ()
-- main = putStrLn "Hello, Haskeller!"
-- main = do
--   greeting <- greetUser
--   putStrLn greeting
--   putStrLn "Your number is: "
--   num <- getLuckyNumber
--   print num
