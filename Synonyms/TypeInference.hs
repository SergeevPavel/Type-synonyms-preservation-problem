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

data Lit = LInt Integer
         | LBool Bool
         deriving (Show, Eq)

data Exp = EVar String
         | ECon String
         | ELit Lit
         | EApp Term Term
         | EAbs String Term
         deriving (Show, Eq)

data Term = Term
        { termExp    :: Exp
        , termType   :: Maybe Type
        , termIsEndo :: Maybe Bool
        } deriving (Show, Eq)


data Scheme = Scheme [String] Type

instance Types Scheme where
    ftv (Scheme vars ty) = ftv ty `Set.difference` Set.fromList vars

    apply su (Scheme vars ty) = Scheme vars (apply (foldr removeFromSubst su vars) ty)

data TypeEnv = TypeEnv (Map.Map String Scheme)

instance Types TypeEnv where
    ftv (TypeEnv env) = ftv (Map.elems env)
    apply su (TypeEnv env) = TypeEnv $ Map.map (apply su) env

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

data TI a = ExceptT String (ReaderT TypeEnv (State Int) a)

ti :: (MonadError String m) => Term -> (m Term)
ti = undefined
