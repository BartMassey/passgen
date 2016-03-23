module Main where

import Control.Applicative
import System.Environment
import System.Exit
import System.Random

main = getArgs >>= parseArgs

-- Generate a random n long IO String. The s IO String is appended to the end
-- result.
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

-- Call generatePassword with s read as an Int.
-- Return help and exit with failure if s cannot be read as an Int.
go :: String -> (IO String, ExitCode)
go s = case reads s :: [(Int, String)] of
        [(n, _)] -> (generatePassword n (return ""), ExitSuccess)
        _ -> (return help, ExitFailure 1)

-- Print the String and exit with ExitCode
outputAndExit :: (String, ExitCode) -> IO ()
outputAndExit t = putStrLn (fst t) >> exitWith (snd t)

-- Print the IO String and exit with ExitCode
outputAndExit' :: (IO String, ExitCode) -> IO ()
outputAndExit' t = fst t >>= putStrLn >> exitWith (snd t)

-- Parse the commandline arguments and return one of:
--   help
--   version
--   actual program output
parseArgs :: [String] -> IO ()
parseArgs ("-l":n:[])   = outputAndExit' (go n)
parseArgs ["-v"]        = outputAndExit (version, ExitSuccess)
parseArgs ["--version"] = outputAndExit (version, ExitSuccess)
parseArgs []            = outputAndExit (help, ExitSuccess)
parseArgs ["-h"]        = outputAndExit (help, ExitSuccess)
parseArgs ["--help"]    = outputAndExit (help, ExitSuccess)
parseArgs _             = outputAndExit (help, ExitFailure 1)

-- Yields a random char from the validChars list.
randChar :: IO Char
randChar = fmap (validChars !!) $ getStdRandom (randomR (0, (length validChars) - 1))

-- A list of valid characters
validChars :: String
validChars = "abcdefghjkmnpqqrstuvxyzABCDEFGHJKLMNPQRSTUVXYZ123456789_-!#=+/"

-- Print the version
version :: String
version = "0.0.4"
