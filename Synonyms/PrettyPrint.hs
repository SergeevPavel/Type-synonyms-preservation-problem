module PrettyPrint where

import Type

import qualified Text.PrettyPrint as PP


instance Show Type where
    showsPrec _ x = shows (prType x)

prType :: Type -> PP.Doc
prType (TVar n)       =   PP.text n
prType (t :-> s)      =   prParenType t PP.<+> PP.text "->" PP.<+> prType s
prType TInt           =   PP.text "Int"
prType TBool          =   PP.text "Bool"
prType TChar          =   PP.text "Char"
prType (TList t)      =   PP.brackets $ prType t
prType (TPair tl tr)  =   PP.parens $ prType tl PP.<+> PP.comma PP.<+> prType tr
prType (TSynonym s t) =   (PP.parens $ prType t) PP.<+> (PP.braces $ PP.text "aka" PP.<+> PP.text s)

prParenType     ::  Type -> PP.Doc
prParenType  t  =   case t of
                      _ :-> _ -> PP.parens (prType t)
                      _       -> prType t
