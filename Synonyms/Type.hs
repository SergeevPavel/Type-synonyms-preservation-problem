module Type where

data Type = TVar String
          | Type :-> Type
          | TInt
          | TBool
          | TList Type
          | TPair Type Type
          | TSynonym String
          deriving (Show, Eq)

data SynonymCtx = Map String Type



