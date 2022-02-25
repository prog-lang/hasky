module Assembly
  ( generate
  , Assembly
  , Instruction
  ) where

import           Opcode                         ( Opcode(..) )
import           Parser                         ( Def(Def, PubDef)
                                                , Module(modBody)
                                                , defName
                                                )

-- EXPORTED

newtype Instruction = Instruction (Opcode, Int)

cmd :: Opcode -> Int -> Instruction
cmd opcode operand = Instruction (opcode, operand)

instance Show Instruction where
  show (Instruction (opcode, operand)) =
    padding ++ show opcode ++ " " ++ show operand ++ "\n"
   where
    padding = replicate offset ' '
    offset  = 2

newtype Assembly = Assembly [Instruction]

instance Show Assembly where
  show (Assembly instructions) = concatMap show instructions

generate :: Module -> Assembly
generate mod = Assembly
  [cmd Closure 0, cmd Apply int, cmd Call 0, cmd Return 0]
 where
  entrypoint = head . filter isMain . Parser.modBody
  isMain     = (== "main") . Parser.defName
  int        = case entrypoint mod of
    Def    _ i -> i
    PubDef _ i -> i
