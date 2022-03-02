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

data Operand = Name String | Int Int

instance Show Operand where
  show (Name name) = name
  show (Int  int ) = show int

newtype Instruction = Instruction (Opcode, Maybe Operand)

instance Show Instruction where
  show (Instruction (opcode, operand)) =
    padding ++ show opcode ++ case operand of
      Just value -> " " ++ show value ++ "\n"
      Nothing    -> "\n"
   where
    padding = replicate offset ' '
    offset  = 2

newtype Assembly = Assembly [Instruction]

instance Show Assembly where
  show (Assembly instructions) = concatMap show instructions

justOpcode :: Opcode -> Instruction
justOpcode opcode = Instruction (opcode, Nothing)

withName :: Opcode -> String -> Instruction
withName opcode name = Instruction (opcode, Just $ Name name)

withInt :: Opcode -> Int -> Instruction
withInt opcode int = Instruction (opcode, Just $ Int int)

generate :: Module -> Assembly
generate mod = Assembly
  [ withName Closure "print"
  , withInt Apply int
  , justOpcode Call
  , justOpcode Return
  ]
 where
  entrypoint = head . filter isMain . Parser.modBody
  isMain     = (== "main") . Parser.defName
  int        = case entrypoint mod of
    Def    _ i -> i
    PubDef _ i -> i
