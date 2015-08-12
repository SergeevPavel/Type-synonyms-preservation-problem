module TypeInference where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Type
import Substitution

data Lit = LInt Integer
         | LBool Bool
         deriving (Show, Eq)

data Exp = EVar String
         | ECon String
         | ELit Lit
         | EApp Term Term
         | EAbs String Term
         deriving (Show, Eq)

data Term = Term {
        termExp    :: Exp,
        termType   :: Maybe Type,
        termIsEndo :: Maybe Bool
        }
        deriving (Show, Eq)


data Scheme = Scheme [String] Type
