module Main where

import           Control.Applicative
import           Data.Default
import           Data.List                      ( isPrefixOf )
import           Debug.Trace                    ( trace )
import           Lexer
import           Parser
import           Test.Tasty
import           Test.Tasty.HUnit

-- MAIN

main :: IO ()
main =
  defaultMain $ testGroup "Parser tests" [tcParserCombinators, tcParseModule]

-- TODO: rewrite these for the new Parser type:
--       [tcParseMod, tcParseUse, tcParseConstDefinition]

-- UTIL

lexparse :: Parser a -> String -> Consumed a
lexparse parser = parse parser . tokenize

-- TEST CASES

tcParserCombinators :: TestTree
tcParserCombinators = testGroup
  "Parser Combinators"
  [ testGroup
    "Normal tests"
    [ testCase "The 'token' combinator"
      $ let parser               = token (TokenType def)
            Consumed (Ok _ rest) = parse parser [TokenType def]
        in  null rest @?= True
    , testCase "The (>>=) combinator"
      $ let switch (TokenUse def) = token (TokenName def "")
            kwpars = token (TokenUse def)
            parser = kwpars >>= switch
            Consumed (Ok (TokenName def name) []) =
              parse parser [TokenUse def, TokenName def "name"]
        in  name @?= "name"
    ]
  , testGroup
    "Destructive test"
    [ testCase "The (<|>) combinator (simple)"
      $ let
          user   = token (TokenUse def)
          moder  = token (TokenMod def)
          parser = user <|> moder
          Empty (Error (Message pos err)) =
            parse parser [TokenLet $ AlexPn 0 1 0]
        in
          pos @?= (1, 0)
    , testCase "The (<|>) combinator"
      $ let typedec =
              tokens [TokenType def, TokenTypeName def def] <?> ("typedec" ++)
            pubdef = tokens [TokenPub def, TokenDef def] <?> ("pubdef" ++)
            either = typedec <|> pubdef
            Consumed (Error (Message _ err)) =
              parse either [TokenType def, TokenName def def]
        in  isPrefixOf "typedec" err @?= True
    , testCase "The 'token' combinator"
      $ let parser = token (TokenType def) <?> ("typetok" ++)
            Empty (Error (Message _ err)) =
              parse parser [TokenTypeName def def]
        in  isPrefixOf "typetok" err @?= True
    , testCase "The 'tokens' combinator"
      $ let typedec =
              tokens [TokenType def, TokenTypeName def def] <?> ("typedec" ++)
            Consumed (Error (Message _ err)) =
              parse typedec [TokenType def, TokenName def def]
        in  isPrefixOf "typedec" err @?= True
    ]
  ]

{-
tcParseMod :: TestTree
tcParseMod = testGroup
  "Parse 'mod' declaration"
  [ testCase
    "EOF"
    (let Left (Error pos err) =
           lexparse Parser.modDeclaration "mod myfancymodule"
     in  null err @?= False
    )
  , testCase
    "Type instead of name"
    (let Left (Error pos err) = lexparse Parser.modDeclaration "mod Core;"
     in  null err @?= False
    )
  , testCase
    "Name begins with a colon"
    (let Left (Error pos err) = lexparse Parser.modDeclaration "mod :core:io;"
     in  null err @?= False
    )
  , testCase
    "Double-colon in the middle of the name"
    (let Left (Error pos err) = lexparse Parser.modDeclaration "mod core::io;"
     in  null err @?= False
    )
  , testCase
    "Normal"
    (let Right ([modname], _) = lexparse Parser.modDeclaration "mod core;"
     in  modname @?= "core"
    )
  , testCase
    "Normal but scoped"
    (let Right (modname, _) = lexparse Parser.modDeclaration "mod core:io;"
     in  modname @?= ["core", "io"]
    )
  ]

tcParseUse :: TestTree
tcParseUse = testGroup
  "Parse 'use' statement"
  [ testCase
    "EOF"
    (let Left (Error pos err) =
           lexparse Parser.useDeclaration "use myfancymodule"
     in  null err @?= False
    )
  , testCase
    "Type instead of name"
    (let Left (Error pos err) = lexparse Parser.useDeclaration "use Core;"
     in  null err @?= False
    )
  , testCase
    "Normal and plain"
    (let Right (JustUse [modname], _) =
           lexparse Parser.useDeclaration "use core;"
     in  modname @?= "core"
    )
  , testCase
    "Normal but scoped"
    (let Right (JustUse scoped, _) =
           lexparse Parser.useDeclaration "use core:io;"
     in  scoped @?= ["core", "io"]
    )
  , testCase
    "Normal but scoped and renamed"
    (let Right (result, _) =
           lexparse Parser.useDeclaration "use core:io as io;"
     in  result @?= UseAs ["core", "io"] "io"
    )
  ]

tcParseConstDefinition :: TestTree
tcParseConstDefinition = testGroup
  "Parse const definition"
  [ testCase
    "EOF"
    (let Left (Error pos err) =
           lexparse Parser.definition "pub def magic := 42"
     in  null err @?= False
    )
  , testCase
    "Type name in identifier"
    (let Left (Error pos err) =
           lexparse Parser.definition "pub def Magic := 42;"
     in  null err @?= False
    )
  , testCase
    "TokenEqual instead of TokenAssign"
    (let Left (Error pos err) =
           lexparse Parser.definition "pub def magic = 42;"
     in  null err @?= False
    )
  , testCase
    "Normal (Public Definition)"
    (let Right (result, _) = lexparse Parser.definition "pub def magic := 42;"
     in  result @?= PubDef "magic" 42
    )
  , testCase
    "Normal (Private Definition)"
    (let Right (result, _) = lexparse Parser.definition "def magic := 42;"
     in  result @?= Def "magic" 42
    )
  ]
-}

tcParseModule :: TestTree
tcParseModule = testGroup
  "Parse first increment of a module"
  [ testGroup
    "Normal tests"
    [ testCase
        "Normal: mod + use + def"
        (let Consumed (Ok result []) = lexparse
               Parser.modParser
               "mod alex:vic; use core:io; def magic := 42;"
         in  result @?= Parser.Module ["alex", "vic"]
                                      [JustUse ["core", "io"]]
                                      [Def "magic" 42]
        )
    ]
  , testGroup
    "Destructive tests"
    [ testCase
      "Missing module declaration"
      (let Empty (Error (Message _ err)) =
             lexparse Parser.modParser "use core:io; def magic := 42;"
       in  null err @?= False
      )
    , testCase
      "Use statement after definition"
      (let Consumed (Error (Message _ err)) = lexparse
             Parser.modParser
             "mod alex:vic; def magic := 42; use core:io;"
       in  null err @?= False
      )
    ]
  ]

-- TODO: make aux func that reads a file with hasky program for tests.
-- TODO: case of that provides a more accurate error explanation instead of
--       non-exhaustive patterns.
