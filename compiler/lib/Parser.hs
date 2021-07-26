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
import qualified Lexer

-- ERROR TYPE

data Error = Error (Int, Int) String

instance Alternative (Either Error) where
  empty = Left $ Error (0, 0) "Empty error."
  Left _ <|> e2 = e2
  e1     <|> _  = e1

-- PARSER TYPE

newtype Parser a = Parser
  { parse :: [Token] -> Either Error (a, [Token])
  }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> do
    (x, input') <- p input
    return (f x, input')

instance Applicative Parser where
  pure value = Parser $ \input -> Right (value, input)
  (Parser p1) <*> (Parser p2) = Parser $ \input -> do
    (f, input' ) <- p1 input
    (a, input'') <- p2 input'
    return (f a, input'')

instance Alternative Parser where
  empty = Parser $ const empty
  (Parser p1) <|> (Parser p2) = Parser $ \input -> p1 input <|> p2 input

instance Monad Parser where
  p >>= f = Parser $ \input -> do
    (a, input') <- parse p input
    parse (f a) input'

instance MonadFail Parser where
  fail message = Parser $ \_ -> Left $ Error (0, 0) message

-- PRIMITIVE PARSERS

token :: Token -> Parser Token
token tok = Parser aux
 where
  aux (t : toks)
    | t == tok
    = Right (t, toks)
    | otherwise
    = Left
      $  Error (tokenPosition t)
      $  "I was looking for '"
      ++ show tok
      ++ "', but found '"
      ++ show t
      ++ "' at "
      ++ Lexer.showTokenPosition t
      ++ ".\n"
  aux [] =
    Left
      $  Error (0, 0)
      $  "If you see this error, it means that Viktor really messed up with\n"
      ++ "the parser... Please open an issue at "
      ++ "https://github.com/sharpvik/hasky/issues, citing the following:\n\n"
      ++ "Expected token: "
      ++ show tok
      ++ "\nBut token stream was exhausted.\n\n"
      ++ "PARSER ERROR #1\n"

tokens :: [Token] -> Parser [Token]
tokens = traverse token

{- Match a list of tokens with a mandatory semicolon at the end of it. -}
declaration :: Parser a -> Parser a
declaration = (<* token (TokenSemicolon def))

sepBy
  ::
  -- Parser for the separators
     Parser a
  ->
  -- Parser for elements
     Parser b
  -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element)

-- SUPREME PARSERS

type ScopedName = [String] -- e.g. core:io = ["core", "io"]

type Mod = ScopedName

scopedName :: Parser ScopedName
scopedName = map unpackString
  <$> sepBy (token $ TokenColon def) (token $ TokenName def def)

modDeclaration :: Parser Mod
modDeclaration = Parser $ \input -> case parse parser input of
  Left (Error pos err) ->
    Left
      $  Error pos
      $  "On my way through parsing the top-level mod declaration,\n"
      ++ err
      ++ "\n> Are you sure you didn't use a capital letter in the name?\n"
      ++ "> Did you forget a semicolon?\n"
      ++ "\nHere's an example of a valid mod declaration:\n\n"
      ++ "    mod myfancymodule;\n"
  right -> right
  where parser = declaration $ token (TokenMod def) *> scopedName

data Use
  = JustUse ScopedName -- use lib;
  | UseAs ScopedName String -- use core:io as io;
  deriving (Show, Eq)

useDeclaration :: Parser Use
useDeclaration = Parser $ \input ->
  case parse (declaration (useAs <|> justUse)) input of
    Left (Error pos err) ->
      Left
        $  Error pos
        $  "On my way through parsing your use statement,\n"
        ++ err
        ++ "\n> Check formatting in this use statement!"
        ++ "\n> Make sure you didn't forget a semicolon at the end!\n"
        ++ "\nHere's a few examples of correct use statements:\n"
        ++ "\n    use core:io as io;"
        ++ "\n    use core:io;"
        ++ "\n    use myfancymodule;\n"
    right -> right
 where
  usePath = token (TokenUse def) *> scopedName
  justUse = JustUse <$> usePath
  useAs =
    UseAs
      <$> usePath
      <*> (unpackString <$> (token (TokenAs def) *> token (TokenName def def)))

data Def
  = Def String Int
  | PubDef String Int
  deriving (Show, Eq)

definition :: Parser Def
definition = Parser $ \input -> case parse parser input of
  Left (Error pos err) ->
    Left
      $  Error pos
      $  "On my way through parsing a definition,\n"
      ++ err
      ++ "\n> Make sure you used correct keywords!"
      ++ "\n> Did you forget a semicolon?\n"
      ++ "\nHere's an example of a valid definition:\n"
      ++ "\n    def magic := 42;\n"
      ++ "\nPublic definitions are available from other modules:\n"
      ++ "\n    pub def shared := 666;\n"
  right -> right
 where
  defParser    = Def <$ token (TokenDef def)
  pubDefParser = PubDef <$ tokens [TokenPub def, TokenDef def]
  nameParser =
    unpackString <$> token (TokenName def def) <* token (TokenAssign def)
  intParser = unpackInt <$> token (TokenInt def def)
  parser =
    declaration $ (pubDefParser <|> defParser) <*> nameParser <*> intParser

data Module = Module
  { modName :: Mod
  , modUses :: [Use]
  , modBody :: [Def]
  }
  deriving (Show, Eq)

modParser :: Parser Module
modParser =
  Module <$> modDeclaration <*> many useDeclaration <*> many definition <* token
    (TokenEOF def)

-- ANALYZE

analyze :: Show a => Parser a -> String -> String
analyze parser input = case parse parser $ tokenize input of
  Left  (Error pos err) -> highlight pos input ++ err
  Right (parsed, _)     -> show parsed
