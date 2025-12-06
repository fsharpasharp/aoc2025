module Solution where

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec, errorBundlePretty, parse)
import Text.Megaparsec.Char (space)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

parseOrDie :: Parser a -> Text -> a
parseOrDie p t =
  case parse p "" t of
    Left e -> error (errorBundlePretty e)
    Right x -> x

integer :: Parser Integer
integer = L.signed space L.decimal