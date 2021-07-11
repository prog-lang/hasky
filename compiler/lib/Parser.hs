{-# LANGUAGE FlexibleInstances #-}

module Parser 
    ( Parser
    , Error(..)
    , Mod
    , analyze
    ) where

import           Control.Applicative
import qualified Lexer 
import           Lexer (Token, tokenPosition)



-- ERROR TYPE


newtype Error = Error String


instance Alternative (Either Error) where
    empty = Left $ Error "Empty error."
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
    fail message = Parser $ \_ -> Left $ Error message



-- PRIMITIVE PARSERS


token :: Token -> Parser Token
token tok = Parser aux where 
    aux (t:toks)
        | t == tok = Right (t, toks)
        | otherwise = Left $ Error $
            "I was looking for '" ++ show tok
            ++ "', but found '" ++ show t ++ "' at "
            ++ Lexer.showTokenPosition t ++ "."
    aux [] = Left $ Error $
         "I was looking for '" ++ show tok
        ++ "', but instead you made me stare into the void...\n"
        ++ "And now that void is staring right back at you!"


tokens :: String -> [Token] -> Parser [Token]
tokens expressionType expect = Parser $ \input ->
    case parse (traverse token expect) input of
        Left (Error err) -> Left $ Error $
            "On my way through parsing " ++ expressionType ++ ",\n" ++ err
        result -> result



-- SUPREME PARSERS


newtype Mod = Mod String deriving Show


modDeclaration :: Parser Mod
modDeclaration = convert
   <$> (tokens "the top-level mod declaration"
    $ [ Lexer.TokenMod $ Lexer.AlexPn 0 0 0
      , Lexer.TokenName (Lexer.AlexPn 0 0 0) ""
      , Lexer.TokenSemicolon $ Lexer.AlexPn 0 0 0
      ])
    where convert [_, Lexer.TokenName _ name, _] = Mod name



-- ANALYZE


analyze :: String -> Either Error (Mod, [Token])
analyze = parse parser . Lexer.tokenize where
    parser = modDeclaration