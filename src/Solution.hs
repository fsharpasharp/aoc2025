module Solution where

import Data.Array (Array, bounds, (!))
import Data.Text (Text)
import Data.List (transpose)
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

prettyArrayChar :: Array (Int,Int) Char -> String
prettyArrayChar arr =
  let ((r0,c0),(r1,c1)) = bounds arr
  in concat
       [ [ arr ! (r,c) | c <- [c0..c1] ]
         ++ (if r < r1 then "\n" else "")
       | r <- [r0..r1]
       ]