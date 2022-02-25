module Opcode where

data Opcode
  = Closure
  | Task0
  | Task1
  | Task2
  | Task3
  | Task4
  | Task5
  | Task6
  | Task7
  | Task8
  | Task9
  | Apply
  | Charge
  | Call
  | Push
  | Return

instance Show Opcode where
  show Closure = "closure"
  show Task0   = "task0"
  show Task1   = "task1"
  show Task2   = "task2"
  show Task3   = "task3"
  show Task4   = "task4"
  show Task5   = "task5"
  show Task6   = "task6"
  show Task7   = "task7"
  show Task8   = "task8"
  show Task9   = "task9"
  show Apply   = "apply"
  show Charge  = "charge"
  show Call    = "call"
  show Push    = "push"
  show Return  = "return"

instruction :: Opcode -> Int -> String
instruction opcode int = padding ++ show opcode ++ " " ++ show int
 where
  padding = replicate offset ' '
  offset  = 2
