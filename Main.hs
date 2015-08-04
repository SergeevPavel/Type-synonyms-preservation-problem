module TypeInference where

data Exp = EVar String
         | ELit Lit
         | EApp Exp Exp
         | EAbs String Exp
