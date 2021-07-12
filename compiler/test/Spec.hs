module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Lexer
import Parser



-- MAIN


main :: IO ()
main = defaultMain $ testGroup "Parser tests"
    [ tcParseMod
    , tcParseUse
    , tcParseConstDefinition
    ]



-- UTIL


lexparse :: Parser a -> String -> Either Error (a, [Token])
lexparse parser = parse parser . tokenize



-- TEST CASES


tcParseMod :: TestTree
tcParseMod = testGroup "Parse 'mod' declaration"
    [ testCase "EOF"
        (let Left (Error err) = 
                 lexparse Parser.modDeclaration "mod myfancymodule"
          in null err @?= False)
    , testCase "Type instead of name"
        (let Left (Error err) = 
                 lexparse Parser.modDeclaration "mod Core;"
          in null err @?= False)
    , testCase "Name begins with a colon"
        (let Left (Error err) = 
                 lexparse Parser.modDeclaration "mod :core:io;"
          in null err @?= False)
    , testCase "Double-colon in the middle of the name"
        (let Left (Error err) = 
                 lexparse Parser.modDeclaration "mod core::io;"
          in null err @?= False)
    , testCase "Normal"
        (let Right ([modname], _) = 
                 lexparse Parser.modDeclaration "mod core;"
          in modname @?= "core")
    , testCase "Normal but scoped"
        (let Right (modname, _) = 
                 lexparse Parser.modDeclaration "mod core:io;"
          in modname @?= ["core", "io"])
    ]


tcParseUse :: TestTree
tcParseUse = testGroup "Parse 'use' statement"
    [ testCase "EOF"
        (let Left (Error err) = 
                 lexparse Parser.useDeclaration "use myfancymodule"
          in null err @?= False)
    , testCase "Type instead of name"
        (let Left (Error err) = 
                 lexparse Parser.useDeclaration "use Core;"
          in null err @?= False)
    , testCase "Normal and plain"
        (let Right (JustUse [modname], _) = 
                 lexparse Parser.useDeclaration "use core;"
          in modname @?= "core")
    , testCase "Normal but scoped"
        (let Right (JustUse scoped, _) = 
                 lexparse Parser.useDeclaration "use core:io;"
          in scoped @?= ["core", "io"])
    , testCase "Normal but scoped and renamed"
        (let Right (result, _) = 
                 lexparse Parser.useDeclaration "use core:io as io;"
          in result @?= UseAs ["core", "io"] "io")
    ]


tcParseConstDefinition :: TestTree
tcParseConstDefinition = testGroup "Parse const definition"
    [ testCase "EOF"
        (let Left (Error err) = 
                 lexparse Parser.definition "pub def magic := 42"
          in null err @?= False)
    , testCase "Normal"
        (let Right (result, _) = 
                 lexparse Parser.definition "pub def magic := 42;"
          in result @?= PubDef "magic" (Int 42))
    ]
