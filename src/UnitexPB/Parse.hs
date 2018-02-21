module UnitexPB.Parse where

import           UnitexPB.Type

import           Control.Applicative
import           Control.Monad
import           Data.Void

import qualified Text.Megaparsec as M
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = M.Parsec Void Stream

---
-- top-level parsers
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
classInst = do
  c <- charsNot (Just "Class") "+ :"
  case c of
    "N" -> noun
    "A" -> adjective
    "DET" -> determiner
    "PREP" -> return PREP
    "CONJ" -> return CONJ
    "PRO" -> pronoun
    "V" -> verb
    "ADV" -> return ADV
    "PFX" -> return PFX
    "SIGL" -> return SIGL
    "ABREV" -> abrev
    "INTERJ" -> return INTERJ

genericClass :: Parser Class
genericClass = do
  c:ts <- classTraces
  ps <- params `startBy1` symbol ":"
  return $ GenericClass (c, ts, ps)

classTraces :: Parser [Stream]
classTraces = charsNot (Just "Classe+Traços") "+: " `M.sepBy` char '+'

params :: Parser Stream
params = charsNot (Just "Params") " :"

---
-- class parsers
noun :: Parser Class
noun = mkClass N $ liftM3 (,,) degree gender number

adjective :: Parser Class
adjective = mkClass A $ liftM3 (,,) degree gender number

determiner :: Parser Class
determiner = do
  symbol "+"
  dt <- article <|> numeral M.<?> "determiner type"
  return dt

article :: Parser Class
article = do
  symbol "Art"
  symbol "+"
  at <- definite <|> indefinite M.<?> "article type"
  symbol ":"
  g <- gender
  n <- number
  return $ DETArt at g n
  where
    definite   = strToData "Def" Def
    indefinite = strToData "Ind" Indef

numeral :: Parser Class
numeral = do
  symbol "Num"
  symbol ":"
  nt <-
    cardinal <|> ordinal <|> multiplicative <|> fractional <|>
    collective M.<?> "numeral type"
  g <- gender
  n <- number
  return $ DETNum nt g n
  where
    cardinal       = strToData "C" Card
    ordinal        = strToData "O" Ordin
    multiplicative = strToData "M" Mult
    fractional     = strToData "F" Frac
    collective     = strToData "L" Collect

pronoun :: Parser Class
pronoun = do
  symbol "PRO"
  symbol "+"
  pt <-
    demons <|> indef <|> relat <|> interr <|> treat <|> poss <|>
    personal M.<?> "pronoun type"
  symbol ":"
  mc <-
    optional
      (acc <|> dat <|> nom <|> obl <|> refl M.<?> "pronoun case")
  p <- person
  g <- gender
  n <- number
  return $ PRO pt mc p g n
  where
    -- type
    demons   = strToData "Dem" Dem
    indef    = strToData "Ind" Ind
    relat    = strToData "Rel" Rel
    interr   = strToData "Int" Interr
    treat    = strToData "Tra" Tra
    poss     = strToData "Pos" Pos
    personal = strToData "Pes" Pes
    -- case
    acc      = strToData "A" Acc
    dat      = strToData "D" Dat
    nom      = strToData "N" Nom
    obl      = strToData "O" Obl
    refl     = strToData "R" Refl

verb :: Parser Class
verb = do
  symbol ":"
  vt <-
    inf <|> ger <|> part <|> pres <|> preti <|> pretp <|> futpinf <|>
    pretmp <|> press <|> imps <|> futs <|> imp <|> futp
    M.<?> "verb form"
  p <- person
  n <- number
  return $ V vt p n
  where
    inf     = strToData "W" VW
    ger     = strToData "G" VG
    part    = strToData "K" VK
    pres    = strToData "P" VP
    preti   = strToData "I" VI
    pretp   = strToData "J" VJ
    futpinf = strToData "F" VF
    pretmp  = strToData "Q" VQ
    press   = strToData "S" VS
    imps    = strToData "T" VT
    futs    = strToData "U" VU
    imp     = strToData "Y" VY
    futp    = strToData "C" VC

abrev :: Parser Class
abrev = liftM2 ABREV (symbol ":" *> gender) number

---
-- param parsers
degree :: Parser (Maybe Degree)
degree = do
  optional (aument <|> dimin <|> superl M.<?> "degree")
  where
    aument = strToData "A" Aument
    dimin  = strToData "D" Dimin
    superl = strToData "S" Superl

gender :: Parser Gender
gender = do
  masc <|> fem M.<?> "gender"
  where
    masc = strToData "m" Masc
    fem  = strToData "f" Fem

number :: Parser Number
number = do
  sg <|> pl M.<?> "number"
  where
    sg = strToData "s" Sg
    pl = strToData "p" Pl

person :: Parser Person
person = do
  p1 <|> p2 <|> p3 M.<?> "person"
  where
    p1 = strToData "1" P1
    p2 = strToData "2" P2
    p3 = strToData "3" P3

---
-- utility parsers
charsNot :: Maybe String -> Stream -> Parser Stream
charsNot l cs = lexeme $ M.takeWhileP l (\c -> c `notElem` cs)

strToData :: String -> a -> Parser a
strToData s t = symbol s *> return t

mkClass :: ([b] -> a) -> Parser b -> Parser a
mkClass c p = do
  ps <- p `startBy1` symbol ":"
  return $ c ps

---
-- parser combinators
startBy1 :: MonadPlus m => m a -> m sep -> m [a]
startBy1 p sep = do
  some (sep >> p)
{-# INLINE startBy1 #-}

---
-- lexing
symbol :: Stream -> Parser Stream
symbol = L.symbol ws

lexeme :: Parser Stream -> Parser Stream
lexeme = L.lexeme ws

ws :: Parser ()
ws = void $ M.takeWhileP (Just "space") (== ' ')
