{-# LANGUAGE FlexibleInstances #-}

module Parser where

import Control.Applicative
import Data.Default (def)
import Highlight (highlight)
import Lexer (Token (..), tokenPosition, tokenize, unpackInt, unpackString)
import qualified Lexer

-- ERROR TYPE

data Error = Error (Int, Int) String

instance Alternative (Either Error) where
  empty = Left $ Error (0, 0) "Empty error."
  Left _ <|> e2 = e2
  e1 <|> _ = e1

-- PARSER TYPE

newtype Parser a = Parser
  { parse :: [Token] -> Either Error (a, [Token])
  }

instance Functor Parser where
  fmap f (Parser p) =
    Parser $ \input -> do
      (x, input') <- p input
      return (f x, input')

instance Applicative Parser where
  pure value = Parser $ \input -> Right (value, input)
  (Parser p1) <*> (Parser p2) =
    Parser $ \input -> do
      (f, input') <- p1 input
      (a, input'') <- p2 input'
      return (f a, input'')

instance Alternative Parser where
  empty = Parser $ const empty
  (Parser p1) <|> (Parser p2) =
    Parser $ \input -> p1 input <|> p2 input

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
      | t == tok = Right (t, toks)
      | otherwise =
        Left $
          Error (tokenPosition t) $
            "I was looking for '" ++ show tok
              ++ "', but found '"
              ++ show t
              ++ "' at "
              ++ Lexer.showTokenPosition t
              ++ ".\n"
    aux [] =
      Left $
        Error (0, 0) $
          "I was looking for '" ++ show tok
            ++ "', but instead you made me stare into the void...\n"
            ++ "And now that void is staring right back at you!\n"

tokens :: [Token] -> Parser [Token]
tokens = traverse token

{- Match a list of tokens with a mandatory semicolon at the end of it. -}
declaration :: Parser a -> Parser a
declaration = (<* token (TokenSemicolon def))

sepBy ::
  Parser a -> -- Parser for the separators
  Parser b -> -- Parser for elements
  Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element)

-- SUPREME PARSERS

type ScopedName = [String] -- e.g. core:io = ["core", "io"]

type Mod = ScopedName

scopedName :: Parser ScopedName
scopedName =
  map convert
    <$> sepBy (token $ TokenColon def) (token $ TokenName def def)
  where
    convert (TokenName _ name) = name

modDeclaration :: Parser Mod
modDeclaration = Parser $ \input ->
  case parse parser input of
    Left (Error pos err) ->
      Left $
        Error pos $
          "On my way through parsing the top-level mod declaration,\n" ++ err
            ++ "\n> Are you sure you didn't use a capital letter in the name?\n"
            ++ "> Did you forget a semicolon?\n"
            ++ "\nHere's an example of a valid mod declaration:\n\n"
            ++ "    mod myfancymodule;\n"
    right -> right
  where
    parser = declaration $ token (TokenMod def) *> scopedName

data Use
  = JustUse ScopedName -- use lib;
  | UseAs ScopedName String -- use core:io as io;
  deriving (Show, Eq)

justUse :: Parser Use
justUse = JustUse <$> (token (TokenUse def) *> scopedName)

useAs :: Parser Use
useAs = Parser $ \input -> do
  (scoped, input') <- parse scopedNameParser input
  (rename, input'') <- parse renameParser input'
  Right (UseAs scoped rename, input'')
  where
    scopedNameParser = token (TokenUse def) *> scopedName
    renameParser =
      renameConverter
        <$> ( token (TokenAs def)
                *> token (TokenName def def)
            )
    renameConverter (TokenName _ name) = name

useDeclaration :: Parser Use
useDeclaration = Parser $ \input ->
  case parse (declaration (useAs <|> justUse)) input of
    Left (Error pos err) ->
      Left $
        Error pos $
          "On my way through parsing your use statement,\n" ++ err
            ++ "\n> Check formatting in this use statement!"
            ++ "\n> Make sure you didn't forget a semicolon at the end!\n"
            ++ "\nHere's a few examples of correct use statements:\n"
            ++ "\n    use core:io as io;"
            ++ "\n    use core:io;"
            ++ "\n    use myfancymodule;\n"
    right -> right

data Def
  = Def String Int
  | PubDef String Int
  deriving (Show, Eq)

definition :: Parser Def
definition = Parser $ \input ->
  case parse parser input of
    Left (Error pos err) ->
      Left $
        Error pos $
          "On my way through parsing a def,\n" ++ err
            ++ "\n>Make sure you used correct keywords!\n"
            ++ "\n>Did you forget a semicolon?\n"
            ++ "\nHere's an example of a correct use statements:\n"
            ++ "\n    pub def magic := 42;\n"
    right -> right
  where
    defParser = Def <$ token (TokenDef def)
    pubDefParser = PubDef <$ tokens [TokenPub def, TokenDef def]
    nameParser = unpackString <$> token (TokenName def def) <* token (TokenAssign def)
    intParser = unpackInt <$> token (TokenInt def def)
    parser =
      declaration $
        (defParser <|> pubDefParser)
          <*> nameParser
          <*> intParser

-- ANALYZE

analyze :: String -> String
analyze input =
  case parse parser $ tokenize input of
    Left (Error pos err) -> highlight pos input ++ err
    Right (parsed, _) -> show parsed
  where
    parser = modDeclaration
