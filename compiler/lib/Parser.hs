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

-- PARSER TYPE

data Message = Message (Int, Int) String

data Reply a = Ok a [Token] | Error Message

data Consumed a = Consumed (Reply a) | Empty (Reply a)

newtype Parser a = Parser
  { parse :: [Token] -> Consumed a
  }

class ErrorInjector a where
  (<?>) :: a -> (String -> String) -> a

instance ErrorInjector Message where
  (<?>) (Message pos msg) inject = Message pos $ inject msg

instance ErrorInjector (Reply a) where
  (<?>) (Ok x ys  ) = const $ Ok x ys
  (<?>) (Error msg) = Error . (<?>) msg

instance ErrorInjector (Parser a) where
  (Parser p) <?> inject = Parser $ \input -> case p input of
    Empty    reply -> Empty $ reply <?> inject
    Consumed reply -> Consumed $ reply <?> inject

instance Show Message where
  show (Message pos err) = err

instance Show a => Show (Reply a) where
  show (Error msg ) = show msg
  show (Ok reply _) = show reply

instance Show a => Show (Consumed a) where
  show (Consumed reply) = show reply
  show (Empty    reply) = show reply

instance Monad Parser where
  return x = Parser $ Empty . Ok x
  (Parser p) >>= f = Parser $ \input -> case p input of
    Empty    (Ok x rest) -> parse (f x) rest
    Empty    (Error msg) -> Empty $ Error msg
    Consumed reply       -> Consumed $ case reply of
      Ok x rest -> case parse (f x) rest of
        Consumed reply' -> reply'
        Empty    reply' -> reply'
      Error msg -> Error msg

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> case p input of
    Empty    (Error msg) -> Empty $ Error msg
    Consumed (Error msg) -> Consumed $ Error msg
    Empty    (Ok x rest) -> Empty $ Ok (f x) rest
    Consumed (Ok x rest) -> Consumed $ Ok (f x) rest

instance Applicative Parser where
  pure value = Parser $ \input -> Empty $ Ok value input
  (Parser p) <*> (Parser q) = Parser $ \input -> case p input of
    Empty    (Error msg) -> Empty $ Error msg
    Consumed (Error msg) -> Consumed $ Error msg
    Empty    (Ok f rest) -> case q rest of
      Empty    (Error msg ) -> Empty $ Error msg
      Consumed (Error msg ) -> Consumed $ Error msg
      Empty    (Ok x rest') -> Empty $ Ok (f x) rest'
      Consumed (Ok x rest') -> Consumed $ Ok (f x) rest'
    Consumed (Ok f rest) -> case q rest of
      Empty    (Error msg ) -> Empty $ Error msg
      Consumed (Error msg ) -> Consumed $ Error msg
      Empty    (Ok x rest') -> Empty $ Ok (f x) rest'
      Consumed (Ok x rest') -> Consumed $ Ok (f x) rest'

-- | Alternative implementation prevents infinite backtracking by checking
--   whether Parser p consumes any input. If p consumed input but errored out,
--   it signifies to us that its error is the one that should be used, not the
--   error from Parser q.
--
--   On top of that nice property, the 'longest match' rule is followed by
--   preferring the successful reply from Parser q if it consumed from input
--   while Parser p did not.
instance Alternative Parser where
  empty = Parser $ const $ Empty $ Error $ Message (0, 0) "Empty error."
  (Parser p) <|> (Parser q) = Parser $ \input -> case p input of
    Empty (Error msg) -> q input
    Empty ok          -> case q input of
      Empty _  -> Empty ok
      consumed -> consumed
    consumed -> consumed

-- PRIMITIVE PARSERS

token :: Token -> Parser Token
token tok = Parser aux
 where
  aux (t : toks)
    | t == tok
    = Consumed $ Ok t toks
    | otherwise
    = Empty
      $  Error
      $  Message (tokenPosition t)
      $  "I was looking for '"
      ++ show tok
      ++ "', but found '"
      ++ show t
      ++ "'.\n"
  aux [] =
    Empty
      $  Error
      $  Message (0, 0)
      $  "If you see this error, it means that Viktor really messed up with\n"
      ++ "the parser... Please open an issue at "
      ++ "https://github.com/sharpvik/hasky/issues, citing the following:\n\n"
      ++ "Expected token: "
      ++ show tok
      ++ "\nBut token stream was exhausted.\n\n"
      ++ "PARSER ERROR #1\n"

tokens :: [Token] -> Parser [Token]
tokens = traverse token

-- | Match a list of tokens with a mandatory semicolon at the end of it.
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
modDeclaration = parser <?> errorTransform
 where
  parser = declaration $ token (TokenMod def) *> scopedName
  errorTransform err =
    "On my way through parsing the top-level mod declaration,\n"
      ++ err
      ++ "\n> Are you sure you didn't use a capital letter in the name?\n"
      ++ "> Did you forget a semicolon?\n"
      ++ "\nHere's an example of a valid mod declaration:\n\n"
      ++ "    mod myfancymodule;\n"

data Use
  = JustUse ScopedName -- use lib;
  | UseAs ScopedName String -- use core:io as io;
  deriving (Show, Eq)

useDeclaration :: Parser Use
useDeclaration = declaration (useAs <|> justUse) <?> errorTransform
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

data Def
  = Def String Int
  | PubDef String Int
  deriving (Show, Eq)

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
  Consumed reply -> interpret reply
  Empty    reply -> interpret reply
 where
  interpret (Error (Message pos err)) = highlight pos input ++ err
  interpret (Ok parsed _            ) = show parsed
