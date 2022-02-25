{-# LANGUAGE RecordWildCards #-}

module Main where

import           Data.List                      ( intercalate )
import           Data.Version                   ( showVersion )
import qualified Parser
import           Paths_hasky_compile            ( version )
import qualified Pipeline
import           System.Environment             ( getArgs )
import           Text.Printf                    ( printf )

-- MAIN

main :: IO ()
main = getArgs >>= mode

-- MODE

mode :: [String] -> IO ()
mode ["help"   ] = usage
mode ["version"] = putStrLn fullVersion
mode [file     ] = Pipeline.file file
mode []          = Pipeline.standard
mode _           = usage

-- INFO

name :: String
name = "hasky"

fullVersion :: String
fullVersion = name ++ " v" ++ showVersion version

usage :: IO ()
usage = printf "Usage: %s [ %s ]\n" name (intercalate " | " commands)

commands :: [String]
commands = ["help", "version", "SOURCE.ha"]
