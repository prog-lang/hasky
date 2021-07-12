{-# LANGUAGE RecordWildCards #-}

module Main where

import           Paths_hasky (version)
import           Data.Version (showVersion)
import           System.Environment (getArgs)
import           Text.Printf (printf)
import           Data.List (intercalate)
import qualified Lexer
import qualified Parser



-- MAIN


main :: IO ()
main = getArgs >>= mode



-- MODE


mode :: [String] -> IO ()
mode ["help"]    = usage
mode ["version"] = putStrLn fullVersion
mode [file]      = readFile file >>= (showResult . Parser.analyze)


showResult:: Either Parser.Error (Parser.Mod, [Lexer.Token]) -> IO ()
showResult(Left (Parser.Error explanation)) = putStrLn explanation
showResult(Right (m, _)) = print m



-- INFO


name = "hasky"

fullVersion = name ++ " v" ++ (showVersion version)

usage = printf "Usage: %s [%s]" name (intercalate " | " commands)

commands = ["help", "version", "SOURCE.ha"]

