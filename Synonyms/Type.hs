module Type where

data Type = TVar String
          | Type :-> Type
          | TInt
          | TBool
          | TChar
          | TList Type
          | TPair Type Type
          | TSynonym String Type -- Synonyms without type variables
          deriving (Eq)

infixr 7 :->

isTypeSynonym :: Type -> Bool
isTypeSynonym (TSynonym _ _) = True
isTypeSynonym _              = False
