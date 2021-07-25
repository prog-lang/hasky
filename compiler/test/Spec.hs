module Main where

import           Debug.Trace                    ( trace )
import           Lexer
import           Parser
import           Test.Tasty
import           Test.Tasty.HUnit

-- MAIN

main :: IO ()
main = defaultMain $ testGroup
  "Parser tests"
  [tcParseMod, tcParseUse, tcParseConstDefinition, tcParseModule]

-- UTIL

lexparse :: Parser a -> String -> Either Error (a, [Token])
lexparse parser = parse parser . tokenize

-- TEST CASES

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

tcParseModule :: TestTree
tcParseModule = testGroup
  "Parse first increment of a module"
  [ testGroup
    "Normal Tests"
    [ testCase
        "Name, 1 import and one def"
        (let Right (result, _) = lexparse
               Parser.modParser
               "mod alex:vic; use core:io; def magic := 42;"
         in  result @?= Parser.Module ["alex", "vic"]
                                      [JustUse ["core", "io"]]
                                      [Def "magic" 42]
        )
    ]
  , testGroup
    "Destructive Tests"
    [ testCase
      "Missing module declaration"
      (let Left (Error pos err) =
             lexparse Parser.modParser "use core:io; def magic := 42;"
       in  null err @?= False
      )
    , testCase
      "Use statement after Def"
      (assertLeft $ lexparse Parser.modParser
                             "mod alex:vic; def magic := 42; use core:io;"
      )
    ]
  ]

-- TODO: make aux func that reads a file with hasky program for tests
assertLeft result = case result of
  Left (Error pos err) -> null err @?= False
  Right (result, _) ->
    assertFailure $ "Expected error, but got output: " ++ show result

-- TODO: case of that provides a more accurate error explanation instead of exhaustive patterns
