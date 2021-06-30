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

  ":"               { \pos _ -> TokenColon      pos          }
  ";"               { \pos _ -> TokenSemicolon  pos          }

  "("               { \pos _ -> TokenLeftParen  pos          }
  ")"               { \pos _ -> TokenRightParen pos          }
  "["               { \pos _ -> TokenLeftBrace  pos          }
  "]"               { \pos _ -> TokenRightBrace pos          }

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

  @type             { \pos s -> TokenType       pos s        }
  @name             { \pos s -> TokenName       pos s        }
  @string           { \pos s -> TokenStr        pos (read s) }
  @integer          { \pos s -> TokenInt        pos (read s) } 
  @op               { \pos s -> TokenOp         pos s        }

{ 
-- TOKEN


data Token
  = TokenLambda AlexPosn
  | TokenDot AlexPosn
  | TokenColon AlexPosn
  | TokenSemicolon AlexPosn
  | TokenLeftParen AlexPosn
  | TokenRightParen AlexPosn
  | TokenLeftBrace AlexPosn
  | TokenRightBrace AlexPosn
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
  | TokenType AlexPosn String
  | TokenName AlexPosn String
  | TokenStr AlexPosn String
  | TokenInt AlexPosn Int
  | TokenOp AlexPosn String
  deriving (Show, Eq) 



-- ALIAS


tokenize :: String -> [Token]
tokenize = alexScanTokens
}
