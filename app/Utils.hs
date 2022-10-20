module Utils (isValidInput) where

isValidInput :: Char -> Bool
isValidInput 'B' = True
isValidInput 'G' = True
isValidInput 'Y' = True
isValidInput 'R' = True
isValidInput 'W' = True
isValidInput 'K' = True
isValidInput  _  = False
