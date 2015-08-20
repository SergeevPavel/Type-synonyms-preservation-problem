{-# LANGUAGE FlexibleContexts #-}

module TypeInference where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Trans.Except
import Control.Monad.Error.Class

import Type
import Substitution

--Expression type
data Lit = LInt Integer
         | LBool Bool
         deriving (Show, Eq)

data Expr = EVar String
          | ECon String
          | ELit Lit
          | EApp Expr Expr
          | EAbs String Expr
          deriving (Show, Eq)

data Term = Term
        { termExp    :: Expr
        , termType   :: Maybe Type
        , termIsEndo :: Maybe Bool
        } deriving (Show, Eq)

-- Scheme type
data Scheme = Scheme [String] Type

instance Types Scheme where
    ftv (Scheme vars ty) = ftv ty `Set.difference` Set.fromList vars

    apply su (Scheme vars ty) = Scheme vars (apply (foldr removeFromSubst su vars) ty)

-- Type environment type
data TypeEnv = TypeEnv (Map.Map String Scheme)

instance Types TypeEnv where
    ftv (TypeEnv env) = ftv (Map.elems env)
    apply su (TypeEnv env) = TypeEnv $ Map.map (apply su) env

removeFromTypeEnv :: String -> TypeEnv -> TypeEnv
removeFromTypeEnv var (TypeEnv env) =  TypeEnv $ Map.delete var env

lookupInTypeEnv :: String -> TypeEnv -> (Maybe Scheme)
lookupInTypeEnv n (TypeEnv env) = Map.lookup n env

addToTypeEnv :: String -> Scheme -> TypeEnv -> TypeEnv
addToTypeEnv n t (TypeEnv env) = TypeEnv $ Map.insert n t env

fromListTypeEnv :: [(String, Scheme)] -> TypeEnv
fromListTypeEnv l = TypeEnv $ Map.fromList l

-- Type inference context
type TI a = ExceptT String (ReaderT TypeEnv (State Int)) a

newTyVar :: String -> TI Type
newTyVar prefix = do
    s <- get
    modify (+ 1)
    return (TVar (prefix ++ show s))

instantiate :: Scheme -> TI Type
instantiate (Scheme vars t) = do
    nvars <- mapM (\ _ -> newTyVar "a") vars
    let s = fromListSubst (zip vars nvars)
    return $ apply s t

-- Unification
mgu :: (MonadError String m) => Type -> Type -> (m Subst)
mgu (l :-> r) (l' :-> r') = do
    su1 <- mgu l l'
    su2 <- mgu (apply su1 r) (apply su1 r')
    return (su1 `composeSubst` su2)
mgu (TVar v) t = varBind v t
mgu t (TVar v) = varBind v t
mgu (TSynonym _ tl) tr = mgu tl tr
mgu tl (TSynonym _ tr) = mgu tl tr
mgu TInt TInt = return nullSubst
mgu TBool TBool = return nullSubst
mgu (TList t) (TList t') = mgu t t'
mgu (TPair t1 t2) (TPair t1' t2') = do
    su1 <- mgu t1 t1'
    su2 <- mgu (apply su1 t2) (apply su1 t2')
    return (su1 `composeSubst` su2)

varBind :: (MonadError String m) => String -> Type -> (m Subst)
varBind u t  | t == TVar u           =  return nullSubst
             | u `Set.member` ftv t  =  throwError $ "occur check fails: " ++ u ++
                                         " vs. " ++ show t
             | otherwise             =  return $ singletonSubst u t

-- Inference
ti :: Expr -> TI (Subst, Type)
ti (EVar n) = do
    env <- ask
    case lookupInTypeEnv n env of
       Nothing     ->  throwError $ "unbound variable: " ++ n
       Just sigma  ->  do  t <- instantiate sigma
                           return (nullSubst, t)
ti (ELit l) = tiLit l
ti (EAbs n e) = do
        tv <- newTyVar "a"
        env <- ask
        let env' = env
        (s1, t1) <- local (addToTypeEnv n (Scheme [] tv)) (ti e)
        let t2 = apply s1 tv
        return (s1, t2 :-> t1)
ti (EApp e1 e2) = do
        tv <- newTyVar "a"
        (s1, t1) <- ti e1
        (s2, t2) <- local (apply s1) (ti e2)
        s3 <- mgu (apply s2 t1) (t2 :-> tv)
        return (s3 `composeSubst` s2 `composeSubst` s1, apply s3 tv)
--ti env (ELet x e1 e2) =
--    do  (s1, t1, e1') <- ti env e1
--        let TypeEnv env' = remove env x
--            t' = generalize (apply s1 env) t1
--            env'' = TypeEnv (Map.insert x t' env')
--        (s2, t2, e2') <- ti (apply s1 env'') e2
--        return (s1 `composeSubst` s2, t2, ELet x e1' e2')

tiLit :: Lit -> TI (Subst, Type)
tiLit (LInt _)   =  return (nullSubst, TInt)
tiLit (LBool _)  =  return (nullSubst, TBool)

-- Runner
--typeInference :: TypeEnv -> Expr -> (TypeEnv, Type)
--typeInference env expr = runStateT (runReaderT (runExceptT (ti expr)) env) 0
