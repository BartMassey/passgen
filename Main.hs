module Main where

import Control.Monad
import System.Environment
import System.Exit
import System.Random

main :: IO ()
main = getArgs >>= parseArgs

-- The valid characters. The password will be constructed from this pool
validChars :: String
validChars = "abcdefghjkmnpqqrstuvxyzABCDEFGHJKLMNPQRSTUVXYZ123456789_-!#=+/"

-- The help text
help :: String
help = unlines [ "Usage: passgen OPTION",
                 "  -l x outputs a random password of x length",
                 "  -h this help text",
                 "  -v version of the program" ]

-- Print the String and exit with ExitCode
outputAndExit :: (String, ExitCode) -> IO ()
outputAndExit (s,e) = putStrLn s >> exitWith e

-- Try to read the given string as a value of the given type.
maybeRead :: Read a => String -> Maybe a
maybeRead s =
    case reads s of
      [(v, [])] -> Just v
      _ -> Nothing

-- Parse the commandline arguments and produce:
--   help
--   version
--   actual program output
parseArgs :: [String] -> IO ()
parseArgs ["-l", nStr]   =
    case maybeRead nStr of
      Just n -> randString n >>= putStrLn
      Nothing -> outputAndExit (help, ExitFailure 1)
parseArgs ["-v"]        = outputAndExit (version, ExitSuccess)
parseArgs ["--version"] = outputAndExit (version, ExitSuccess)
parseArgs []            = outputAndExit (help, ExitSuccess)
parseArgs ["-h"]        = outputAndExit (help, ExitSuccess)
parseArgs ["--help"]    = outputAndExit (help, ExitSuccess)
parseArgs _             = outputAndExit (help, ExitFailure 1)

randChar :: IO Char
randChar = (validChars !!) <$> randomRIO (0, length validChars - 1)

-- Generate a 'n' long random IO String.
randString :: Int -> IO String
randString n = replicateM n randChar

-- Print the version
version :: String
version = "0.0.6"
