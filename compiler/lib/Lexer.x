{ 
module Lexer where 

import Data.Default
}

%wrapper "posn" 

@integer = [0-9]+
@char    = ' $printable{1,2} '
@string  = \" ($printable # \")* \"
@name    = [a-z] [a-zA-Z0-9]*
@type    = [A-Z] [a-zA-Z0-9]*
@op      = [=!\|\/\+\-\*\$\<\>]+

tokens :-
    $white+           ; 
    "--".*            ; 
    
    "\"               { \pos _ -> TokenLambda     pos          }
    "."               { \pos _ -> TokenDot        pos          }
    ","               { \pos _ -> TokenComma      pos          }
    "|"               { \pos _ -> TokenPipe       pos          }
    ":"               { \pos _ -> TokenColon      pos          }
    ";"               { \pos _ -> TokenSemicolon  pos          }
    
    "("               { \pos _ -> TokenLeftParen  pos          }
    ")"               { \pos _ -> TokenRightParen pos          }
    "["               { \pos _ -> TokenLeftBrace  pos          }
    "]"               { \pos _ -> TokenRightBrace pos          }
    
    ":-"              { \pos _ -> TokenTypeSign   pos          }
    ":="              { \pos _ -> TokenAssign     pos          }
    
    mod               { \pos _ -> TokenMod        pos          }
    use               { \pos _ -> TokenUse        pos          }
    as                { \pos _ -> TokenAs         pos          }
    pub               { \pos _ -> TokenPub        pos          }
    def               { \pos _ -> TokenDef        pos          }
    let               { \pos _ -> TokenLet        pos          }
    in                { \pos _ -> TokenIn         pos          }
    do                { \pos _ -> TokenDo         pos          }
    end               { \pos _ -> TokenEnd        pos          }
    when              { \pos _ -> TokenWhen       pos          }
    is                { \pos _ -> TokenIs         pos          }
    then              { \pos _ -> TokenThen       pos          }
    type              { \pos _ -> TokenType       pos          }
    alias             { \pos _ -> TokenAlias      pos          }
    
    @type             { \pos s -> TokenTypeName   pos s        }
    @name             { \pos s -> TokenName       pos s        }
    @char             { \pos s -> TokenChar       pos (read s) }
    @string           { \pos s -> TokenStr        pos (read s) }
    @integer          { \pos s -> TokenInt        pos (read s) } 
    @op               { \pos s -> TokenOp         pos s        }

{ 
-- TOKEN


data Token
    = TokenLambda AlexPosn
    | TokenDot AlexPosn
    | TokenComma AlexPosn
    | TokenPipe AlexPosn
    | TokenColon AlexPosn
    | TokenSemicolon AlexPosn
    | TokenLeftParen AlexPosn
    | TokenRightParen AlexPosn
    | TokenLeftBrace AlexPosn
    | TokenRightBrace AlexPosn
    | TokenTypeSign AlexPosn
    | TokenAssign AlexPosn
    | TokenMod AlexPosn
    | TokenUse AlexPosn
    | TokenAs AlexPosn
    | TokenPub AlexPosn
    | TokenDef AlexPosn
    | TokenLet AlexPosn
    | TokenIn AlexPosn
    | TokenDo AlexPosn
    | TokenEnd AlexPosn
    | TokenWhen AlexPosn
    | TokenIs AlexPosn
    | TokenThen AlexPosn
    | TokenType AlexPosn
    | TokenAlias AlexPosn
    | TokenTypeName AlexPosn String
    | TokenName AlexPosn String
    | TokenChar AlexPosn Char
    | TokenStr AlexPosn String
    | TokenInt AlexPosn Int
    | TokenOp AlexPosn String


unpackString :: Token -> String
unpackString (TokenTypeName _ str) = str
unpackString (TokenName _ str) = str
unpackString (TokenStr _ str) = str
unpackString (TokenOp _ str) = str


unpackInt :: Token -> Int
unpackInt (TokenInt _ int) = int


instance Show Token where
    show (TokenLambda _) = "\\"
    show (TokenDot _) = "."
    show (TokenComma _) = ","
    show (TokenPipe _) = "|"
    show (TokenColon _) = ":"
    show (TokenSemicolon _) = ";"
    show (TokenLeftParen _) = "("
    show (TokenRightParen _) = ")"
    show (TokenLeftBrace _) = "]"
    show (TokenRightBrace _) = "["
    show (TokenTypeSign _) = ":-"
    show (TokenAssign _) = ":="
    show (TokenMod _) = "mod"
    show (TokenUse _) = "use"
    show (TokenAs _) = "as"
    show (TokenPub _) = "pub"
    show (TokenDef _) = "def"
    show (TokenLet _) = "let"
    show (TokenIn _) = "in"
    show (TokenDo _) = "do"
    show (TokenEnd _) = "end"
    show (TokenWhen _) = "when"
    show (TokenIs _) = "is"
    show (TokenThen _) = "then"
    show (TokenType _) = "type"
    show (TokenAlias _) = "alias"
    show (TokenTypeName _ name) = "type name"
    show (TokenName _ _) = "name"
    show (TokenChar _ char) = "character"
    show (TokenStr _ str) = "string"
    show (TokenInt _ int) = "integer"
    show (TokenOp _ op) = "operator"


instance Eq Token where
    TokenLambda _ == TokenLambda _ = True
    TokenDot _ == TokenDot _ = True
    TokenComma _ == TokenComma _ = True
    TokenPipe _ == TokenPipe _ = True
    TokenColon _ == TokenColon _ = True
    TokenSemicolon _ == TokenSemicolon _ = True
    TokenLeftParen _ == TokenLeftParen _ = True
    TokenRightParen _ == TokenRightParen _ = True
    TokenLeftBrace _ == TokenLeftBrace _ = True
    TokenRightBrace _ == TokenRightBrace _ = True
    TokenTypeSign _ == TokenTypeSign _ = True
    TokenAssign _ == TokenAssign _ = True
    TokenMod _ == TokenMod _ = True
    TokenUse _ == TokenUse _ = True
    TokenAs _ == TokenAs _ = True
    TokenPub _ == TokenPub _ = True
    TokenDef _ == TokenDef _ = True
    TokenLet _ == TokenLet _ = True
    TokenIn _ == TokenIn _ = True
    TokenDo _ == TokenDo _ = True
    TokenEnd _ == TokenEnd _ = True
    TokenWhen _ == TokenWhen _ = True
    TokenIs _ == TokenIs _ = True
    TokenThen _ == TokenThen _ = True
    TokenType _ == TokenType _ = True
    TokenAlias _ == TokenAlias _ = True
    TokenTypeName _ _ == TokenTypeName _ _ = True
    TokenName _ _ == TokenName _ _ = True
    TokenChar _ _ == TokenChar _ _ = True
    TokenStr _ _ == TokenStr _ _ = True
    TokenInt _ _ == TokenInt _ _ = True
    TokenOp _ _ == TokenOp _ _ = True
    _ == _ = False



-- ALIAS


tokenize :: String -> [Token]
tokenize = alexScanTokens



-- POSITION


instance Default AlexPosn where
    def = AlexPn 0 0 0


position :: AlexPosn -> (Int, Int)
position (AlexPn _ line column) = (line, column)


tokenPosition :: Token -> (Int, Int)
tokenPosition (TokenLambda pos) = position pos
tokenPosition (TokenDot pos) = position pos
tokenPosition (TokenComma pos) = position pos
tokenPosition (TokenPipe pos) = position pos
tokenPosition (TokenColon pos) = position pos
tokenPosition (TokenSemicolon pos) = position pos
tokenPosition (TokenLeftParen pos) = position pos
tokenPosition (TokenRightParen pos) = position pos
tokenPosition (TokenLeftBrace pos) = position pos
tokenPosition (TokenRightBrace pos) = position pos
tokenPosition (TokenTypeSign pos) = position pos
tokenPosition (TokenAssign pos) = position pos
tokenPosition (TokenMod pos) = position pos
tokenPosition (TokenUse pos) = position pos
tokenPosition (TokenAs pos) = position pos
tokenPosition (TokenPub pos) = position pos
tokenPosition (TokenDef pos) = position pos
tokenPosition (TokenLet pos) = position pos
tokenPosition (TokenIn pos) = position pos
tokenPosition (TokenDo pos) = position pos
tokenPosition (TokenEnd pos) = position pos
tokenPosition (TokenWhen pos) = position pos
tokenPosition (TokenIs pos) = position pos
tokenPosition (TokenThen pos) = position pos
tokenPosition (TokenType pos) = position pos
tokenPosition (TokenAlias pos) = position pos
tokenPosition (TokenTypeName pos _) = position pos
tokenPosition (TokenName pos _) = position pos
tokenPosition (TokenChar pos _) = position pos
tokenPosition (TokenStr pos _) = position pos
tokenPosition (TokenInt pos _) = position pos
tokenPosition (TokenOp pos _) = position pos


showTokenPosition :: Token -> String
showTokenPosition token = "line " ++ show line ++ ", column " ++ show column 
    where (line, column) = tokenPosition token
}
