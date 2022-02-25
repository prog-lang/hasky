module Pipeline where

import qualified Assembly
import qualified Parser

compile :: String -> String
compile input =
  let result = Parser.analyze Parser.modParser input
  in  case result of
        Left  err -> err
        Right mod -> show $ Assembly.generate mod

standard :: IO ()
standard = interact compile

file :: String -> IO ()
file name = readFile name >>= (putStrLn . compile)
