module Utils where

import           Data.List
import           System.Console.ANSI
import           System.IO

getValidChar :: (Char -> Char) -- input transformation
             -> (Char -> Bool) -- pred applied to trans
             -> IO Char
getValidChar t p = do
  char <- getCharNoEcho
  let newChar = t char
      isValid = p newChar
  if isValid then do
    putChar newChar
    return newChar
  else getValidChar t p

getCharNoEcho :: IO Char
getCharNoEcho = do
  hSetEcho stdin False
  char <- getChar
  hSetEcho stdin True
  return char

allValues :: (Bounded a, Enum a) => [a]
allValues = [minBound..]

-- intersect without duplicates
intersect' :: Eq a => [a] -> [a] -> [a]
intersect' xs ys = xs \\ (xs \\ ys)

partitionEq :: Eq a => [(a, a)] -> ([(a, a)], [(a, a)])
partitionEq = partition (uncurry (==))

-- screen utils

resetScreen :: IO ()
resetScreen = clearScreen >> setCursorPosition 0 0

resetLine :: IO ()
resetLine = clearLine >> setCursorColumn 0 >> cursorUpLine 0
