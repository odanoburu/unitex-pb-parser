module UnitexPB.Type where

-- following description at Appendix A of Muniz, M. C. M. A construção
-- de recursos lingüístico-computacionais para o português do Brasil:
-- o projeto de Unitex-PB. Dissertação de Mestrado. Instituto de
-- Ciências Matemáticas de São Carlos, USP. 72p. 2004. Available at
-- https://web.archive.org/web/20161228212911/http://www.nilc.icmc.usp.br/nilc/projects/unitex-pb/web/files/Formato_DELAF_PB.pdf

type Stream = String

data Entry =
  Entry WordP
        Class
  deriving (Eq, Show)

type WordP = (Stream, Stream)

data Class
  = N (Maybe Degree)
      Gender
      Number
  | A Degree
      Maybe Gender
      Number
  | DETArt DefT
           Gender
           Number
  | PREP
  | CONJ
  | DETNum NumT
           Gender
           Number
  | PRO ProT
        Maybe
        Case
        Person
        Gender
        Number
  | V VType
      Person
      Number
  | ADV
  | PFX
  | SIGL
  | ABREV Gender
          Number
  | INTERJ
  | GenericClass Stream [Stream] [Stream]
  deriving (Eq, Read, Show)

data Gender
  = Masc
  | Fem
  deriving (Eq, Show)

data Number
  = Sg
  | Pl
  deriving (Eq, Show)

data Degree
  = Aument
  | Dimin
  | Superl
  deriving (Eq, Show)

data DefT
  = Def
  | Indef
  deriving (Eq, Show)

data NumT
  = Card
  | Ordin
  | Mult
  | Frac
  | Colect
  deriving (Eq, Show)

data ProT
  = Dem
  | Ind
  | Rel
  | Interr
  | Tra
  | Pos
  | Pes
  deriving (Eq, Show)

data Case
  = Acc
  | Dat
  | Nom
  | Obl
  | Refl
  deriving (Eq, Show)

data Person
  = P1
  | P2
  | P3
  deriving (Eq, Show)

data VType
  = VW
  | VG
  | VK
  | VP
  | VI
  | VJ
  | VF
  | VQ
  | VS
  | VT
  | VU
  | VY
  | VC
  deriving (Eq, Show)

---
-- mk
mkClass :: Stream -> [Stream] -> [Stream] -> Class
mkClass "N" [] = mkNoun
mkClass "A" [] = mkA
mkClass "DET" ("Art":t:[]) = mkArt t
mkClass "DET" ("Num":t:[]) = mkNum t
mkClass "PRO" [t] = mkPro t
mkClass "V" [t] = mkV
mkClass "ABREV" [] = mkAbrev
mkClass c [] [] = read c

mkDGN :: [Stream] -> Class
mkNoun (d:g:n) =
  if null n
    then N (gender d) (number g) Nothing
    else N (gender g) (number n) (degree d)

mkA :: [Stream] -> Class
mkA (d:g:n) =

gender :: Stream -> Gender
gender "m" = Masc
gender "f" = Fem

number :: Stream -> Number
number "s" = Sg
number "p"= Pl

degree :: Stream -> Degree
degree "A" = Aument
degree "D" = Dimin
degree "S" = Superl
