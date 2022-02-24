{-# LANGUAGE RecordWildCards #-}

module Main where

import           Data.List                      ( intercalate )
import           Data.Version                   ( showVersion )
import qualified Lexer
import qualified Parser
import           Paths_hasky                    ( version )
import           System.Environment             ( getArgs )
import           Text.Printf                    ( printf )

-- MAIN

main :: IO ()
main = getArgs >>= mode

-- MODE

mode :: [String] -> IO ()
mode ["help"   ] = usage
mode ["version"] = putStrLn fullVersion
mode ["compile", file] =
  readFile file >>= (putStrLn . Parser.analyze Parser.modParser)
mode _ = usage

-- INFO

name :: String
name = "hasky"

fullVersion :: String
fullVersion = name ++ " v" ++ showVersion version

usage :: IO ()
usage = printf "Usage: %s [%s]\n" name (intercalate " | " commands)

commands :: [String]
commands = ["help", "version", "compile SOURCE.ha"]
