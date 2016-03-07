module Main where

import Control.Applicative
import System.Environment
import System.Exit
import System.Random

main = getArgs >>= parseArgs

-- Exit with success.
exit :: IO a
exit = exitSuccess

-- Generate an x long password. The initial s String is appended to the
-- password.
generatePassword :: Int -> IO String -> IO String
generatePassword n s
  | n < 1 = s
  | otherwise = generatePassword (n -1) (liftA2 (:) randChar s)

-- Print the help text.
help :: String
help = "Usage: passgen OPTION\n" ++
       "  -l x outputs a random password of x length\n" ++
       "  -h this help text\n" ++
       "  -v version of the program"

-- Return "Bad input" if String cannot be interpreted as an Int, else call
-- generatePassword.
go :: String -> IO String
go s = case reads s :: [(Int, String)] of
        [(n, _)] -> generatePassword n (return "")
        _ -> return "Bad input"

-- Parse the commandline arguments and return one of:
--   help
--   version
--   actual program output
parseArgs :: [String] -> IO ()
parseArgs ("-l":n:[])   = go n >>= putStrLn >> exit
parseArgs ["-v"]        = putStrLn version >> exit
parseArgs ["--version"] = putStrLn version >> exit
parseArgs _             = putStrLn help >> exit

-- Yields a random char from the validChars list.
randChar :: IO Char
randChar = fmap (validChars !!) randNum

-- Yields a random integer in the 0 .. length validChars range.
randNum :: IO Int
randNum = getStdRandom (randomR (0, (length validChars) - 1))

-- A list of valid characters
validChars :: String
validChars = "abcdefghjkmnpqqrstuvxyzABCDEFGHJKLMNPQRSTUVXYZ123456789_-!#=+/"

-- Print the version
version :: String
version = "0.0.2"
