module Main where

import           Control.Applicative
import           Data.Default
import           Data.List                      ( isPrefixOf )
import           Debug.Trace                    ( trace )
import           Lexer
import           Parser
import           Parser.Basics
import           Parser.Combinators             ( sepBy
                                                , try
                                                )
import           Parser.Partial                 ( declaration
                                                , eof
                                                , token
                                                , tokens
                                                )
import           Test.Tasty
import           Test.Tasty.HUnit

-- MAIN

main :: IO ()
main = defaultMain $ testGroup
  "Parser tests"
  [tcParserCombinators, tcParseModule, tcParseMod, tcParseUse]

-- TODO: make aux func that reads a file with hasky program for tests.

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
    "Destructive tests"
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

tcParseMod :: TestTree
tcParseMod = testGroup
  "Parse 'mod' declaration"
  -- TODO: quicktests on the weird characters used in module names?
  [ testGroup
    "Destructive Tests"
    [ testCase "EOF" $ assertParseFail Parser.modDeclaration "mod myfancymodule"
    , testCase "Type instead of name"
      $ assertParseFail Parser.modDeclaration "mod Core;"
    , testCase "Name begins with a colon"
      $ assertParseFail Parser.modDeclaration "mod :core:io;"
    , testCase "Double-colon in the middle of the name"
      $ assertParseFail Parser.modDeclaration "mod core::io;"
    ]
  , testGroup
    "Normal Tests"
    [ testCase "Normal"
      $ assertParseOk ignoreTheRest Parser.modDeclaration "mod core;" ["core"]
    , testCase "Normal but scoped" $ assertParseOk ignoreTheRest
                                                   Parser.modDeclaration
                                                   "mod core:io;"
                                                   ["core", "io"]
    ]
  ]

tcParseUse :: TestTree
tcParseUse = testGroup
  "Parse 'use' statement"
  [ testGroup
    "Destructive Tests"
    [ testCase "EOF" $ assertParseFail Parser.useDeclaration "use myfancymodule"
    , testCase "Type instead of name"
      $ assertParseFail Parser.useDeclaration "use Core;"
    ]
  , testGroup
    "Normal Tests"
    [ testCase "Normal and plain" $ assertParseOk ignoreTheRest
                                                  Parser.useDeclaration
                                                  "use core;"
                                                  (JustUse ["core"])
    , testCase "Normal but scoped" $ assertParseOk ignoreTheRest
                                                   Parser.useDeclaration
                                                   "use core:io;"
                                                   (JustUse ["core", "io"])
    , testCase "Normal but scoped and renamed" $ assertParseOk
      ignoreTheRest
      Parser.useDeclaration
      "use core:io as io;"
      (UseAs ["core", "io"] "io")
    ]
  ]

tcParseConstDefinition :: TestTree
tcParseConstDefinition = testGroup
  "Parse const definition"
  [ testGroup
    "Destructive Tests"
    [ testCase "EOF" $ assertParseFail Parser.definition "pub def magic := 42"
    , testCase "Type name in identifier"
      $ assertParseFail Parser.definition "pub def Magic := 42;"
    , testCase "Incorrect assert operator"
      $ assertParseFail Parser.definition "pub def magic = 42;"
    ]
  , testGroup
    "Normal Tests"
    [ testCase "Normal (Public Definition)" $ assertParseOk
      ignoreTheRest
      Parser.definition
      "pub def magic := 42"
      (PubDef "magic" 42)
    , testCase "Normal (Private Definition)" $ assertParseOk ignoreTheRest
                                                             Parser.definition
                                                             "def magic := 42"
                                                             (Def "magic" 42)
    ]
  ]

tcParseModule :: TestTree
tcParseModule = testGroup
  "Parse first increment of a module"
  [ testGroup
    "Normal tests"
    [ testCase "Normal: mod + use + def" $ assertParseOk
        expectNoMore
        Parser.modParser
        "mod alex:vic; use core:io; def magic := 42;"
        (Parser.Module ["alex", "vic"] [JustUse ["core", "io"]] [Def "magic" 42]
        )
    ]
  , testGroup
    "Destructive tests"
    [ testCase "Missing module declaration"
      $ assertParseFail Parser.modParser "use core:io; def magic := 42;"
    , testCase "Use statement after definition" $ assertParseFail
      Parser.modParser
      "mod alex:vic; def magic := 42; use core:io;"
    ]
  ]

-- UTIL

lexparse :: Parser a -> String -> Consumed a
lexparse parser = parse parser . tokenize

assertParseFail :: (Show a) => Parser a -> String -> Assertion
assertParseFail parser input = case lexparse parser input of
  Empty    (Error _) -> True @?= True
  Consumed (Error _) -> True @?= True
  rest ->
    assertFailure $ "Expected an error, but got: " ++ show rest ++ " instead."

assertParseOk
  :: (Show a, Eq a)
  => ([Token] -> Assertion)
  -> Parser a
  -> String
  -> a
  -> Assertion
assertParseOk restVal parser input expected = case lexparse parser input of
  Consumed (Ok parsed rest) ->
    if parsed == expected then restVal rest else parsed @?= expected
  Consumed (Error e) ->
    assertFailure
      $  "Consumed the token succesfully, but got an error: "
      ++ show e
      ++ ", expected: "
      ++ show expected
  Empty (Error e) ->
    assertFailure
      $  "Nothing was consumed and an error shown: "
      ++ show e
      ++ ", while expected output:"
      ++ show expected
  Empty _ ->
    assertFailure $ "Nothing was parsed, while expected: " ++ show expected

expectNoMore :: [Token] -> Assertion
expectNoMore tokens =
  assertBool ("Expected no more tokens to parse, but got: " ++ show tokens)
    $ null tokens

ignoreTheRest :: [Token] -> Assertion
ignoreTheRest _ = True @?= True
