module Utils where

import           Data.Char
import           System.Console.ANSI
import           System.IO

resetScreen :: IO ()
resetScreen = clearScreen 
           >> setCursorPosition 0 0

getValidChar :: IO Char
getValidChar = do
  char <- getCharNoEcho
  let uChar = toUpper char
      valid = isValidChar uChar
  if valid then do
    putChar uChar
    return uChar
  else getValidChar

getCharNoEcho :: IO Char
getCharNoEcho = do
  hSetEcho stdin False
  char <- getChar
  hSetEcho stdin True
  return char

isValidChar :: Char -> Bool
isValidChar 'B' = True
isValidChar 'G' = True
isValidChar 'Y' = True
isValidChar 'R' = True
isValidChar 'W' = True
isValidChar 'K' = True
isValidChar  _  = False
