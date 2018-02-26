module UnitexPB.Parse where

import           UnitexPB.Type

import           Control.Applicative
import           Control.Monad
import           Data.Void

import qualified Text.Megaparsec as M
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Megaparsec.Error

type Parser = M.Parsec Void Stream

---
-- top-level parsers
dictionary :: Parser [Entry]
dictionary = ws *> entry `M.endBy` eol <* M.eof

entry :: Parser Entry
entry = do
  w <- wordP
  symbol "."
  c <- M.withRecovery recoverLine classInst
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
    "N"      -> noun
    "A"      -> adjective
    "DET"    -> determiner
    "PREP"   -> return PREP
    "CONJ"   -> return CONJ
    "PRO"    -> pronoun
    "V"      -> verb
    "ADV"    -> return ADV
    "PFX"    -> return PFX
    "SIGL"   -> return SIGL
    "ABREV"  -> abrev
    "INTERJ" -> return INTERJ
    _ -> empty M.<?> "unknown class"

genericClass :: Parser Class
genericClass = do
  c:ts <- classTraces
  mkClass (GenericClass . (,,) c ts) params

classTraces :: Parser [Stream]
classTraces =
  charsNot (Just "Classe+Traços") "+ :" `M.sepBy` symbol "+"

params :: Parser Stream
params = charsNot (Just "Params") " :\n"

---
-- class parsers
noun :: Parser Class
noun = mkClass N $ liftM3 (,,) degree gender number

adjective :: Parser Class
adjective = mkClass A $ liftM3 (,,) degree gender number

determiner :: Parser Class
determiner = do
  symbol "+"
  article <|> numeral M.<?> "determiner type"

article :: Parser Class
article = do
  symbol "Art+"
  at <- artTp
  mkClass DETArt $ liftM2 ((,,) at) gender number
  where
    definite = strToData "Def" Def
    indefinite = strToData "Ind" Indef
    artTp = definite <|> indefinite M.<?> "article type"

numeral :: Parser Class
numeral = do
  symbol "Num"
  mkClass DETNum $ liftM3 (,,) numTp gender number
  where
    cardinal = strToData "C" Card
    ordinal = strToData "O" Ordin
    multiplicative = strToData "M" Mult
    fractional = strToData "F" Frac
    collective = strToData "L" Collect
    numTp =
      cardinal <|> ordinal <|> multiplicative <|> fractional <|>
      collective M.<?> "numeral type"

pronoun :: Parser Class
pronoun = do
  symbol "PRO+"
  mkClass PRO $ liftM5 (,,,,) proTp caseTp person gender number
  where
    -- type
    demons   = strToData "Dem" Dem
    indef    = strToData "Ind" Ind
    relat    = strToData "Rel" Rel
    interr   = strToData "Int" Interr
    treat    = strToData "Tra" Tra
    poss     = strToData "Pos" Pos
    personal = strToData "Pes" Pes
    proTp    =
      (demons <|> indef <|> relat <|> interr <|> treat <|> poss <|>
       personal M.<?> "pronoun type") <* symbol ":"
    -- case
    acc  = strToData "A" Acc
    dat  = strToData "D" Dat
    nom  = strToData "N" Nom
    obl  = strToData "O" Obl
    refl = strToData "R" Refl
    caseTp =
      optional
        (acc <|> dat <|> nom <|> obl <|> refl M.<?> "pronoun case")

verb :: Parser Class
verb = mkClass V $ liftM3 (,,) verbTp person number
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
    verbTp  =
      inf <|> ger <|> part <|> pres <|> preti <|> pretp <|> futpinf <|>
      pretmp <|> press <|> imps <|> futs <|> imp <|> futp
      M.<?> "verb form"

abrev :: Parser Class
abrev = mkClass ABREV $ liftM2 (,) gender number

---
-- param parsers
degree :: Parser (Maybe Degree)
degree = optional (aument <|> dimin <|> superl M.<?> "degree")
  where
    aument = strToData "A" Aument
    dimin  = strToData "D" Dimin
    superl = strToData "S" Superl

gender :: Parser Gender
gender = masc <|> fem M.<?> "gender"
  where
    masc = strToData "m" Masc
    fem  = strToData "f" Fem

number :: Parser Number
number = sg <|> pl M.<?> "number"
  where
    sg = strToData "s" Sg
    pl = strToData "p" Pl

person :: Parser Person
person = p1 <|> p2 <|> p3 M.<?> "person"
  where
    p1 = strToData "1" P1
    p2 = strToData "2" P2
    p3 = strToData "3" P3

---
-- utility parsers
charsNot :: Maybe String -> Stream -> Parser Stream
charsNot l cs = lexeme $ M.takeWhileP l (`notElem` cs)

strToData :: String -> a -> Parser a
strToData s t = symbol s *> return t

mkClass :: ([b] -> a) -> Parser b -> Parser a
mkClass c p = do
  ps <- p `startBy1` symbol ":"
  return $ c ps

recoverLine :: ParseError t e -> Parser Class
recoverLine pe =
  charsNot (Just "rest of line") "\n" *>
  (return . NotParsed $ show $ errorPos pe)

---
-- parser combinators
startBy1 :: MonadPlus m => m a -> m sep -> m [a]
startBy1 p sep = some (sep >> p)
{-# INLINE startBy1 #-}

---
-- lexing
symbol :: Stream -> Parser Stream
symbol = L.symbol ws

lexeme :: Parser Stream -> Parser Stream
lexeme = L.lexeme ws

ws :: Parser ()
ws = void $ M.takeWhileP (Just "space") (== ' ')

---
-- main
parseMain :: [FilePath] -> IO ()
parseMain fps = do
  ds <- mapM (\file -> M.runParser dictionary file <$> readFile file)  fps
  mapM_ print ds
  return ()
