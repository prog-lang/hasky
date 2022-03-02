module Pipeline where

import System.Exit (die)
import qualified Assembly
import qualified Parser

compile :: String -> Either String String
compile input =
  let result = Parser.analyze Parser.modParser input
  in  case result of
        Left  err -> Left err
        Right mod -> Right $ show $ Assembly.generate mod

compileInteractively :: String -> String
compileInteractively input =
  case compile input of
    Left err -> err
    Right asm -> asm

standard :: IO ()
standard = interact compileInteractively

gracefulExit :: Either String String -> IO ()
gracefulExit result =
  case result of
    Left err -> die err
    Right asm -> putStrLn asm

file :: String -> IO ()
file name = readFile name >>= (gracefulExit . compile)
