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
  readFile file >>= (putStrLn . Parser.analyze Parser.definition)
mode _ = usage

-- INFO

name = "hasky"

fullVersion = name ++ " v" ++ showVersion version

usage = printf "Usage: %s [%s]" name (intercalate " | " commands)

commands = ["help", "version", "SOURCE.ha"]
