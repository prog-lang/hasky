import Paths_hasky (version)
import Data.Version (showVersion)
import System.Environment (getArgs)
import Text.Printf (printf)
import Data.List (intercalate)
import Lexer (tokenize)



-- MAIN


main :: IO ()
main = getArgs >>= mode



-- MODE


mode :: [String] -> IO ()
mode ["help"]    = usage
mode ["version"] = putStrLn fullVersion
mode [file]      = readFile file >>= (print . tokenize)



-- INFO


name = "hasky"

fullVersion = name ++ " v" ++ (showVersion version)

usage = printf "Usage: %s [%s]" name (intercalate " | " commands)

commands = ["help", "version", "SOURCE.ha"]

