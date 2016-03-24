-- Copyright © 2016 Thomas Løcke
-- Password Generator

import Control.Monad
import Data.Char
import Data.List
import System.Console.ParseArgs
import System.Exit
import System.Random
import Text.Printf

data Option = OptionVersion | OptionHelp | OptionLength
            deriving (Ord, Eq, Show)

argd :: [Arg Option]
argd = [
 Arg { argIndex = OptionVersion,
       argName = Just "version",
       argAbbr = Just 'v',
       argData = Nothing,
       argDesc = "Show program version" },
 Arg { argIndex = OptionHelp,
       argName = Just "help",
       argAbbr = Just 'h',
       argData = Nothing,
       argDesc = "Show this usage message" },
 Arg { argIndex = OptionLength,
       argName = Just "length",
       argAbbr = Just 'l',
       argData = argDataDefaulted "char-count" ArgtypeInt 12,
       argDesc = "Number of chars in generated password" } ]

main :: IO ()
main = do
  args <- parseArgsIO ArgsComplete argd
  when (gotArg args OptionVersion) $ do
         printf "%s: version %s\n" (argsProgName args) version
         exitSuccess
  when (gotArg args OptionHelp) $ do
         putStrLn (argsUsage args)
         exitSuccess
  pw <- randString (getRequiredArg args OptionLength)
  putStrLn pw

-- The valid characters. The password will be constructed from this pool
validChars :: String
validChars =
    alphabetics ++ map toUpper alphabetics ++ ['1'..'9'] ++ "_-!#=+/"
    where
      alphabetics = ['a' .. 'z'] \\ ['i', 'o', 'w'] 

-- Generate a random valid Char.
randChar :: IO Char
randChar = (validChars !!) <$> randomRIO (0, length validChars - 1)

-- Generate a 'n' long random IO String.
randString :: Int -> IO String
randString n = replicateM n randChar

-- Print the version
version :: String
version = "0.0.6"
