import System.Directory (listDirectory)
import Data.List (intercalate)
import System.FilePath.Posix (joinPath)
import Lexer (Token, tokenize)



-- MAIN


main :: IO ()
main = tokenizeExamples



-- INFO


examplesFolder = "examples"



-- TOKENIZE


tokenizeExamples :: IO ()
tokenizeExamples 
  = listDirectory examplesFolder
  >>= (mapM_ $ tokenizeFile examplesFolder)


tokenizeFile :: FilePath -> FilePath -> IO ()
tokenizeFile folder path 
  = readFile (joinPath [folder, path])
  >>= (print . tokenize)

