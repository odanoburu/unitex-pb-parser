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
  = N [((Maybe Degree), Gender, Number)]
  | A [((Maybe Degree), Gender, Number)]
  | DETArt [(DefT, Gender, Number)]
  | PREP
  | CONJ
  | DETNum [(NumT, Gender, Number)]
  | PRO [(ProT, (Maybe Case), Person, Gender, Number)]
  | V [(VType, Person, Number)]
  | ADV
  | PFX
  | SIGL
  | ABREV [(Gender, Number)]
  | INTERJ
  | GenericClass ( Stream -- class
                 , [Stream] -- traces
                 , [Stream] -- [params]
                  )
  deriving (Eq, Show)

data Degree
  = Aument
  | Dimin
  | Superl
  deriving (Eq, Show)

data Gender
  = Masc
  | Fem
  deriving (Eq, Show)

data Number
  = Sg
  | Pl
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
  | Collect
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
