module Main ( Exp(..),
              Type(..),
              ti,  -- |ti :: TypeEnv -> Exp -> (Subst, Type)|
              main
            ) where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Monad.Trans.Except
import Control.Monad.Error.Class
import Control.Monad.Reader
import Control.Monad.State

import qualified Text.PrettyPrint as PP

data EndoMarker = Unknown | NotEndo | Endo deriving (Eq, Ord, Show)

data Exp     =  EVar String
             |  ELit Lit
             |  EApp Exp Exp
             |  EAbs EndoMarker String Exp
 --            |  EList List
             |  ELet String Exp Exp
             deriving (Eq, Ord)

data Lit     =  LInt Integer
             |  LBool Bool
             deriving (Eq, Ord)

{-
data List    =  Nil
             |  Cons
             deriving (Eq, Ord)
-}

data Type    =  TVar String
             |  TInt
             |  TBool
             |  TList Type
             |  TFun Type Type
             deriving (Eq, Ord)

data TypeSynonym = TypeSynonym String Type

data Scheme  =  Scheme [String] Type

typeConstructors = Map.fromList [("Nil", Scheme ["a"] $ TList $ TVar "a"),
                                 ("Cons", Scheme ["a"] $ TFun (TVar "a") $ TFun (TList $ TVar "a") (TList $ TVar "a"))]

nil :: Exp
nil = EVar "Nil"

cons :: Exp -> Exp -> Exp
cons head tail = EApp (EApp (EVar "Cons") head) tail

class Types a where
    ftv    ::  a -> Set.Set String
    apply  ::  Subst -> a -> a

instance Types Type where
    ftv (TVar n)      =  Set.singleton n
    ftv TInt          =  Set.empty
    ftv TBool         =  Set.empty
    ftv (TFun t1 t2)  =  ftv t1 `Set.union` ftv t2
    ftv (TList t)     =  ftv t

    apply s (TVar n)      =  case Map.lookup n s of
                               Nothing  -> TVar n
                               Just t   -> t
    apply s (TFun t1 t2)  = TFun (apply s t1) (apply s t2)
    apply s (TList t)     = TList $ apply s t
    apply s t             =  t

instance Types Scheme where
    ftv (Scheme vars t)      =  (ftv t) `Set.difference` (Set.fromList vars)

    apply s (Scheme vars t)  =  Scheme vars (apply (foldr Map.delete s vars) t)

instance Types a => Types [a] where
    apply s  =  map (apply s)
    ftv l    =  foldr Set.union Set.empty (map ftv l)

type Subst = Map.Map String Type

nullSubst  ::  Subst
nullSubst  =   Map.empty

composeSubst         :: Subst -> Subst -> Subst
composeSubst s1 s2   = (Map.map (apply s1) s2) `Map.union` s1

newtype TypeEnv = TypeEnv (Map.Map String Scheme)

remove                    ::  TypeEnv -> String -> TypeEnv
remove (TypeEnv env) var  =  TypeEnv (Map.delete var env)

instance Types TypeEnv where
    ftv (TypeEnv env)      =  ftv (Map.elems env)
    apply s (TypeEnv env)  =  TypeEnv (Map.map (apply s) env)

generalize        ::  TypeEnv -> Type -> Scheme
generalize env t  =   Scheme vars t
  where vars = Set.toList ((ftv t) `Set.difference` (ftv env))

data TIEnv = TIEnv {}

data TIState = TIState { tiSupply :: Int,
                         tiSubst :: Subst }

type TI a = ExceptT String (ReaderT TIEnv (StateT TIState IO)) a

runTI :: TI a -> IO (Either String a, TIState)
runTI t = do
    (res, st) <- runStateT (runReaderT (runExceptT t) initTIEnv) initTIState
    return (res, st)
        where
            initTIEnv = TIEnv {}
            initTIState = TIState{ tiSupply = 0,
                                   tiSubst  = Map.empty }

newTyVar :: String -> TI Type
newTyVar prefix = do
    s <- get
    put s { tiSupply = tiSupply s + 1 }
    return (TVar (prefix ++ show (tiSupply s)))

instantiate :: Scheme -> TI Type
instantiate (Scheme vars t) = do
    nvars <- mapM (\ _ -> newTyVar "a") vars
    let s = Map.fromList (zip vars nvars)
    return $ apply s t

mgu :: Type -> Type -> TI Subst
mgu (TFun l r) (TFun l' r')  =  do
    s1 <- mgu l l'
    s2 <- mgu (apply s1 r) (apply s1 r')
    return (s1 `composeSubst` s2)
mgu (TList t) (TList t')     =  mgu t t'
mgu (TVar u) t               =  varBind u t
mgu t (TVar u)               =  varBind u t
mgu TInt TInt                =  return nullSubst
mgu TBool TBool              =  return nullSubst
mgu t1 t2                    =  throwE $ "types do not unify: " ++ show t1 ++
                                " vs. " ++ show t2

varBind :: String -> Type -> TI Subst
varBind u t  | t == TVar u           =  return nullSubst
             | u `Set.member` ftv t  =  throwE $ "occur check fails: " ++ u ++
                                         " vs. " ++ show t
             | otherwise             =  return (Map.singleton u t)

tiLit :: TypeEnv -> Lit -> TI (Subst, Type, Exp)
tiLit _ lit@(LInt _)   =  return (nullSubst, TInt, ELit lit)
tiLit _ lit@(LBool _)  =  return (nullSubst, TBool, ELit lit)

ti        ::  TypeEnv -> Exp -> TI (Subst, Type, Exp)
ti (TypeEnv env) exp@(EVar n) =
    case Map.lookup n env of
       Nothing     ->  throwE $ "unbound variable: " ++ n
       Just sigma  ->  do  t <- instantiate sigma
                           return (nullSubst, t, exp)
ti env (ELit l) = tiLit env l
ti env (EAbs _ n e) =
    do  tv <- newTyVar "a"
        let TypeEnv env' = remove env n
            env'' = TypeEnv (env' `Map.union` (Map.singleton n (Scheme [] tv)))
        (s1, t1, e') <- ti env'' e
        let t2 = apply s1 tv
        let isEndo = if t1 == t2 then Endo else NotEndo
        return (s1, TFun t2 t1, EAbs isEndo n e')
ti env (EApp e1 e2) =
    do  tv <- newTyVar "a"
        (s1, t1, e1') <- ti env e1
        (s2, t2, e2') <- ti (apply s1 env) e2
        s3 <- mgu (apply s2 t1) (TFun t2 tv)
        return (s3 `composeSubst` s2 `composeSubst` s1, apply s3 tv, EApp e1' e2')
ti env (ELet x e1 e2) =
    do  (s1, t1, e1') <- ti env e1
        let TypeEnv env' = remove env x
            t' = generalize (apply s1 env) t1
            env'' = TypeEnv (Map.insert x t' env')
        (s2, t2, e2') <- ti (apply s1 env'') e2
        return (s1 `composeSubst` s2, t2, ELet x e1' e2')

typeInference :: Map.Map String Scheme -> Exp -> TI (Type, Exp)
typeInference env e =
    do  (s, t, term) <- ti (TypeEnv $ env `Map.union` typeConstructors) e
        return ((apply s t), term)


-- Examples
e0  =  (
        Map.empty,
        ELet "id" (EAbs Unknown "x" (EVar "x"))
         (EVar "id")
       )

e1  =  (
        Map.empty,
        ELet "id" (EAbs Unknown "x" (EVar "x"))
         (EApp (EVar "id") (EVar "id"))
       )

e2  =  (
        Map.empty,
        ELet "id" (EAbs Unknown "x" (ELet "y" (EVar "x") (EVar "y")))
         (EApp (EVar "id") (EVar "id"))
       )

e3  =  (
        Map.empty,
        ELet "id" (EAbs Unknown "x" (ELet "y" (EVar "x") (EVar "y")))
         (EApp (EApp (EVar "id") (EVar "id")) (ELit (LInt 2)))
       )

e4  =  (
        Map.empty,
        ELet "id" (EAbs Unknown "x" (EApp (EVar "x") (EVar "x")))
         (EVar "id")
       )

e5  =  (
        Map.empty,
        EAbs Unknown "m" (ELet "y" (EVar "m")
         (ELet "x" (EApp (EVar "y") (ELit (LBool True)))
         (EVar "x")))
       )

e6  =  (
        Map.empty,
        EAbs Unknown "x" $ EVar "x"
       )

e7  =  (
        Map.fromList [("and", Scheme [] $ TFun TBool (TFun TBool TBool))],
        EAbs Unknown "x" $ EApp (EApp (EVar "and") (EVar "x")) $ EVar "x"
       )

e8  =  (
        Map.empty,
        nil
       )

e9  =  (
        Map.empty,
        cons (ELit $ LInt 42) (cons (ELit $ LInt 43) nil)
       )

e10 =  (
        Map.empty,
        cons (ELit $ LInt 42) (cons (ELit $ LBool True) nil)
       )

e11  =  (
        Map.empty,
        cons (EAbs Unknown "x" $ EVar "x") (cons (EAbs Unknown "y" $ EVar "y") nil)
       )

test :: (Map.Map String Scheme, Exp) -> IO ()
test e =
    do  (res, _) <- runTI (typeInference (fst e) (snd e))
        case res of
          Left err  ->  putStrLn $ "error: " ++ err
          Right t   ->  putStrLn $ show (snd t) ++ " :: " ++ show (fst t)

main :: IO ()
main = mapM_ test [e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11]

-- Pretty printing

instance Show Type where
    showsPrec _ x = shows (prType x)

prType             ::  Type -> PP.Doc
prType (TVar n)    =   PP.text n
prType TInt        =   PP.text "Int"
prType TBool       =   PP.text "Bool"
prType (TFun t s)  =   prParenType t PP.<+> PP.text "->" PP.<+> prType s
prType (TList t)   =   PP.text "List" PP.<+> prType t

prParenType     ::  Type -> PP.Doc
prParenType  t  =   case t of
                      TFun _ _  -> PP.parens (prType t)
                      _         -> prType t

instance Show Exp where
    showsPrec _ x = shows (prExp x)

prExp                  ::  Exp -> PP.Doc
prExp (EVar name)             =   PP.text name
prExp (ELit lit)              =   prLit lit
prExp (ELet x b body)         =   PP.text "let" PP.<+>
                                  PP.text x PP.<+> PP.text "=" PP.<+>
                                  prExp b PP.<+> PP.text "in" PP.$$
                                  PP.nest 2 (prExp body)
prExp (EApp e1 e2)            =   prExp e1 PP.<+> prParenExp e2
prExp (EAbs isEndo n e)       =   PP.char '\\' PP.<+> PP.text (show isEndo) PP.<+> PP.text n PP.<+>
                                  PP.text "->" PP.<+>
                                  prExp e
--prExp (EList (Nil))           =   PP.text "Nil"
--prExp (EList (Cons e1 e2))    =   PP.text "Cons" PP.<+> prExp e1 PP.<+> prExp e2

prParenExp    ::  Exp -> PP.Doc
prParenExp t  =   case t of
                    ELet _ _ _    -> PP.parens (prExp t)
                    EApp _ _      -> PP.parens (prExp t)
                    EAbs _ _ _    -> PP.parens (prExp t)
                    _             -> prExp t

instance Show Lit where
    showsPrec _ x = shows (prLit x)

prLit            ::  Lit -> PP.Doc
prLit (LInt i)   =   PP.integer i
prLit (LBool b)  =   if b then PP.text "True" else PP.text "False"

instance Show Scheme where
    showsPrec _ x = shows (prScheme x)

prScheme                  ::  Scheme -> PP.Doc
prScheme (Scheme vars t)  =   PP.text "All" PP.<+>
                              PP.hcat
                                (PP.punctuate PP.comma (map PP.text vars))
                              PP.<> PP.text "." PP.<+> prType t

