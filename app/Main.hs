module Main where

import           Foo   (greetUser, getLuckyNumber)
import           Types

main :: IO ()
-- main = putStrLn "Hello, Haskeller!"
main = do
  greeting <- greetUser
  putStrLn greeting
  putStrLn "Your number is: "
  num <- getLuckyNumber
  print num
