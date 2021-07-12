{-# LANGUAGE FlexibleInstances #-}

module Parser where

import           Control.Applicative
import qualified Lexer 
import           Lexer (Token(..), tokenPosition, tokenize)
import           Data.Default (def)



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
            ++ Lexer.showTokenPosition t ++ ".\n"
    aux [] = Left $ Error $
         "I was looking for '" ++ show tok
        ++ "', but instead you made me stare into the void...\n"
        ++ "And now that void is staring right back at you!\n"


tokens :: [Token] -> Parser [Token]
tokens = traverse token 


{- Match a list of tokens with a mandatory semicolon at the end of it. -}
declaration :: Parser a -> Parser a
declaration = (<* token (TokenSemicolon def))


sepBy :: Parser a   -- Parser for the separators
      -> Parser b   -- Parser for elements
      -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element)



-- SUPREME PARSERS


type ScopedName = [String]  -- e.g. core:io = ["core", "io"]

type Mod = ScopedName;


modDeclaration :: Parser Mod
modDeclaration = Parser $ \input ->
    case parse parser input of
        Left (Error err) -> Left $ Error $
            "On my way through parsing the top-level mod declaration,\n" ++ err
            ++ "\n> Are you sure you didn't use a capital letter in the name?\n"
            ++ "> Did you forget a semicolon?\n"
            ++ "\nHere's an example of a valid mod declaration:\n\n"
            ++ "    mod myfancymodule;\n"
        right -> right
    where parser = declaration $ token (TokenMod def) *> scopedName



data Use 
    = JustUse ScopedName       -- use lib;
    | UseAs ScopedName String  -- use core:io as io;
    deriving (Show, Eq)


scopedName :: Parser ScopedName
scopedName 
    = map convert
   <$> sepBy (token $ TokenColon def) (token $ TokenName def def)
    where convert (TokenName _ name) = name


justUse :: Parser Use
justUse = JustUse <$> (token (TokenUse def) *> scopedName)


-- sequenceA [ scopedName, token $ TokenAs def, token $ TokenName def def ]
useAs :: Parser Use
useAs = Parser $ \input -> do 
    (scoped, input') <- parse scopedNameParser input
    (rename, input'') <- parse renameParser input'
    Right (UseAs scoped rename, input'')
        where
            scopedNameParser = token (TokenUse def) *> scopedName
            renameParser = renameConverter 
                        <$> (token (TokenAs def) 
                         *> (token $ TokenName def def))
            renameConverter (TokenName _ name) = name


useDeclaration :: Parser Use
useDeclaration = Parser $ \input ->
    case parse (declaration (useAs <|> justUse)) input of
        Left (Error err) -> Left $ Error $
            "On my way through parsing your use statement,\n" ++ err
            ++ "\n> Check formatting in this use statement!"
            ++ "\n> Make sure you didn't forget a semicolon at the end!\n"
            ++ "\nHere's a few examples of correct use statements:\n"
            ++ "\n    use core:io as io;"
            ++ "\n    use core:io;"
            ++ "\n    use myfancymodule;\n"
        right -> right



-- ANALYZE


analyze :: String -> Either Error (Mod, [Token])
analyze = parse parser . tokenize where
    parser = modDeclaration
