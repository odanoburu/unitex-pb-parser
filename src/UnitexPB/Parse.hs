module UnitexPB.Parse where

import           UnitexPB.Type

import           Control.Applicative
import           Control.Monad
import           Data.Void

import qualified Text.Megaparsec as M
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = M.Parsec Void Stream

dictionary :: Parser [Entry]
dictionary = ws *> entry `M.sepBy` eol <* M.eof

entry :: Parser Entry
entry = do
  w <- wordP
  symbol "."
  c <- classInst <|> genericClass
  return $ Entry w c

wordP :: Parser WordP
wordP = do
  w <- charsNot (Just "word") ", "
  symbol ","
  cw <- charsNot (Just "word canonic form") ". "
  return (w, cw)

classInst :: Parser Class


genericClass :: Parser Class
genericClass = do
  c:ts <- classTraces
  symbol ":"
  ps <- params
  return $ GenericClass c ts ps

classTraces :: Parser [Stream]
classTraces = charsNot (Just "Classe+Traços") "+: " `M.sepBy` char '+'

params :: Parser Stream
params = notChar ':' `M.sepBy` char ':'

---
-- utility parsers
charsNot :: Maybe String -> Stream -> Parser Stream
charsNot l cs = lexeme $ M.takeWhileP l (\c -> c `notElem` cs)

---
-- lexing
symbol :: Stream -> Parser Stream
symbol = L.symbol ws

lexeme :: Parser Stream -> Parser Stream
lexeme = L.lexeme ws

ws :: Parser ()
ws = void $ M.takeWhileP (Just "space") (== ' ')
