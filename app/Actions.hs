module Actions where

import           Data.Char     (toUpper)
import           System.IO     (hSetEcho, stdin)
import           System.Random (Random (randomRs), newStdGen)
import           Types         (CodePeg (CodePegB, CodePegK), Secret (..))

-- Generates a random code
-- with a length of four.
generateCode :: IO Secret
generateCode = do
  Secret . take 4 . xs <$> newStdGen
  where xs = randomRs (CodePegB, CodePegK)

-- Echos and returns
-- a valid Char only.
getChar' :: IO Char
getChar' = do

  hSetEcho stdin False
  char <- getChar
  hSetEcho stdin True

  let uChar = toUpper char
      valid = isValid uChar

  if valid then do
    putChar uChar
    return uChar
  else getChar'

  where isValid :: Char -> Bool
        isValid 'B' = True
        isValid 'G' = True
        isValid 'Y' = True
        isValid 'R' = True
        isValid 'W' = True
        isValid 'K' = True
        isValid  _  = False


