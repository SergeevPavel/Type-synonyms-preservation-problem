module Type where

data Type = TVar String
          | Type :-> Type
          | TInt
          | TBool
          | TList Type
          | TPair Type Type
          | TSynonym String Type -- Synonyms without type variables
          deriving (Show, Eq)


