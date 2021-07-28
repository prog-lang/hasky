module Parser.Basics where

import           Control.Applicative
import           Control.Monad                  ( ap )
import           Lexer                          ( Token(..) )

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
  (<*>) = ap

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
