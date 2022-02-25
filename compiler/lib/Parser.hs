{-# LANGUAGE FlexibleInstances #-}

module Parser where

import           Control.Applicative
import           Data.Default                   ( def )
import           Highlight                      ( highlight )
import           Lexer                          ( Token(..)
                                                , tokenPosition
                                                , tokenize
                                                , unpackInt
                                                , unpackString
                                                )
import           Parser.Basics
import           Parser.Combinators             ( sepBy
                                                , try
                                                )
import           Parser.Partial                 ( declaration
                                                , eof
                                                , token
                                                , tokens
                                                )

-- TYPES

type ScopedName = [String] -- e.g. core:io = ["core", "io"]

type Mod = ScopedName

data Use
  = JustUse ScopedName -- use lib;
  | UseAs ScopedName String -- use core:io as io;
  deriving (Show, Eq)

data Def
  = Def String Int
  | PubDef String Int
  deriving (Show, Eq)

defName :: Def -> String
defName (Def    name _) = name
defName (PubDef name _) = name

data Module = Module
  { modName :: Mod
  , modUses :: [Use]
  , modBody :: [Def]
  }
  deriving (Show, Eq)

-- SUPREME PARSERS (BASED ON PARTIAL PARSERS)

scopedName :: Parser ScopedName
scopedName = map unpackString
  <$> sepBy (token $ TokenColon def) (token $ TokenName def def)

modDeclaration :: Parser Mod
modDeclaration = parser <?> errorTransform
 where
  parser = declaration $ token (TokenMod def) *> scopedName
  errorTransform err =
    "On my way through parsing the top-level mod declaration,\n"
      ++ err
      ++ "\n> Are you sure you didn't use a capital letter in the name?"
      ++ "\n> Did you forget a semicolon?\n"
      ++ "\nHere's an example of a valid mod declaration:\n\n"
      ++ "    mod myfancymodule;\n"

useDeclaration :: Parser Use
useDeclaration = declaration (try useAs <|> justUse) <?> errorTransform
 where
  errorTransform err =
    "On my way through parsing your use statement,\n"
      ++ err
      ++ "\n> Check formatting in this use statement!"
      ++ "\n> Make sure you didn't forget a semicolon at the end!\n"
      ++ "\nHere's a few examples of correct use statements:\n"
      ++ "\n    use core:io as io;"
      ++ "\n    use core:io;"
      ++ "\n    use myfancymodule;\n"
  usePath = token (TokenUse def) *> scopedName
  justUse = JustUse <$> usePath
  useAs =
    UseAs
      <$> usePath
      <*> (unpackString <$> (token (TokenAs def) *> token (TokenName def def)))

definition :: Parser Def
definition = parser <?> errorTransform
 where
  errorTransform err =
    "On my way through parsing a definition,\n"
      ++ err
      ++ "\n> Make sure you used correct keywords!"
      ++ "\n> Did you forget a semicolon?\n"
      ++ "\nHere's an example of a valid definition:\n"
      ++ "\n    def magic := 42;\n"
      ++ "\nPublic definitions are available from other modules:\n"
      ++ "\n    pub def shared := 666;\n"
  defParser    = Def <$ token (TokenDef def)
  pubDefParser = PubDef <$ tokens [TokenPub def, TokenDef def]
  nameParser =
    unpackString <$> token (TokenName def def) <* token (TokenAssign def)
  intParser = unpackInt <$> token (TokenInt def def)
  parser =
    declaration $ (pubDefParser <|> defParser) <*> nameParser <*> intParser

modParser :: Parser Module
modParser =
  Module <$> modDeclaration <*> many useDeclaration <*> many definition <* eof

-- ANALYZE

analyze :: Show a => Parser a -> String -> Either String a
analyze parser input = case parse parser $ tokenize input of
  Consumed reply -> interpret reply
  Empty    reply -> interpret reply
 where
  interpret (Error (Message pos err)) = Left $ highlight pos input ++ err
  interpret (Ok parsed _            ) = Right parsed
