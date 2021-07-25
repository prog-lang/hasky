module Highlight
  ( highlight
  ) where

import           Text.Printf                    ( printf )

highlight :: (Int, Int) -> String -> String
highlight (0   , 0  ) _    = "" -- We don't know where the error is.
highlight (line, col) text = "\n" ++ aux (line - 1) (col - 1) text ++ "\n"

aux :: Int -> Int -> String -> String
aux line col text = bord
 where
  idxs = filter (>= 0) [line - 1 .. line]
  lins = map (lines text !!) idxs ++ [replicate col ' ' ++ "^"]
  idxx = map (+ 1) $ idxs ++ [last idxs + 1]
  mail = maximum $ map (length . show) idxx
  bord = unlines $ zipWith frmt idxx lins
  tabs = 4 -- 1 tab = 4 spaces
  padd i = replicate (tabs + mail - length (show i)) ' '
  frmt :: Int -> String -> String
  frmt i l = printf "%s%d | %s" (padd i) i l
