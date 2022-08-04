module Parser.Partial where

import           Control.Applicative
import           Data.Default                   ( def )
import           Lexer                          ( Token(..)
                                                , tokenPosition
                                                )
import           Parser.Basics
import           Parser.Combinators             ( sepBy
                                                , try
                                                )

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
      ++ "https://github.com/prog-lang/hasky/issues, citing the following:\n\n"
      ++ "Expected token: "
      ++ show tok
      ++ "\nBut token stream was exhausted.\n\n"
      ++ "PARSER ERROR #1\n"

tokens :: [Token] -> Parser [Token]
tokens = traverse token

eof :: Parser Token
eof = token (TokenEOF def) <?> const
  (  "I really didn't expect to see anything else here, and yet...\n\n"
  ++ "> Usually this means that you put declarations in the wrong order.\n\n"
  ++ "Hasky expects your module files to look like this:\n\n"
  ++ "    mod myfancymodule; -- this must be the frist thing I see!\n\n"
  ++ "    use core:io as io;\n"
  ++ "    use myotherlib;    -- optional list of use statements\n\n"
  ++ "    pub def mag := 42; -- then the list of definitions\n"
  )

-- | Match a list of tokens with a mandatory semicolon at the end of it.
declaration :: Parser a -> Parser a
declaration = (<* token (TokenSemicolon def))
