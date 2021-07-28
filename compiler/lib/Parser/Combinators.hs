module Parser.Combinators where

import           Control.Applicative
import           Parser.Basics

-- | Convert LL(1) parser into the standard LL(∞) parser.
try :: Parser a -> Parser a
try (Parser p) = Parser $ \input -> case p input of
  Consumed err@(Error _) -> Empty err
  other                  -> other

-- | Parse a list of elements that contain a certain separator between them.
--   For example, hasky module name might be something like "core:io".
sepBy :: Parser separator -> Parser element -> Parser [element]
sepBy separator element = (:) <$> element <*> many (separator *> element)
