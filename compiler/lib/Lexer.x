{ 
module Lexer where 
}

%wrapper "posn" 

@integer = [0-9]+
@string  = \" ($printable # \")* \"
@name    = [a-z] [a-zA-Z0-9]*
@type    = [A-Z] [a-zA-Z0-9]*
@op      = [=!\/\+\-\*\$\<\>]+

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
  | TokenStr AlexPosn String
  | TokenInt AlexPosn Int
  | TokenOp AlexPosn String
  deriving (Show, Eq) 



-- ALIAS


tokenize :: String -> [Token]
tokenize = alexScanTokens
}
