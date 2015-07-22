{-# LANGUAGE FlexibleContexts #-}

module TypeInference where

import Control.Monad.Except
import Data.Maybe
import Data.List

infixl 2 :@
infixr 3 :->

type Symb = String

data Expr =
    Var Symb
    | Expr :@ Expr
    | Lam Symb Expr
    | TrueV
    | FalseV
        deriving (Eq,Show)

data Type =
    TVar Symb
    | Type :-> Type
    | BoolT
        deriving (Eq,Show)

newtype Env = Env [(Symb,Type)]
    deriving (Eq,Show)

newtype SubsTy = SubsTy [(Symb, Type)]
    deriving (Eq,Show)

freeVars :: Expr -> [Symb]
freeVars (Var s) = [s]
freeVars (el :@ er) = freeVars el `union` freeVars er
freeVars (Lam s e) = delete s $ freeVars e
freeVars _ = []

freeTVars :: Type -> [Symb]
freeTVars (TVar s) = [s]
freeTVars (tl :-> tr) = freeTVars tl `union` freeTVars tr
freeTVars _ = []

extendEnv :: Env -> Symb -> Type -> Env
extendEnv (Env vars) s t = Env $ (s, t):vars

freeTVarsEnv :: Env -> [Symb]
freeTVarsEnv (Env vars) = nub $ concatMap (freeTVars . snd) vars

appEnv :: (MonadError String m) => Env -> Symb -> m Type
appEnv (Env vars) s = case lookup s vars of Nothing -> throwError $ getErrorMessage s
                                            Just t  -> return t
    where
        getErrorMessage :: String -> String
        getErrorMessage s = "There is no variable " ++ show s ++ " in the enviroment."

appSubsTy :: SubsTy -> Type -> Type
appSubsTy (SubsTy subst) t@(TVar ts) = fromMaybe t $ lookup ts subst
appSubsTy subst (tl :-> tr) = appSubsTy subst tl :-> appSubsTy subst tr
-- appSubsTy

appSubsEnv :: SubsTy -> Env -> Env
appSubsEnv subst (Env vars) = Env $ map (\(s, t) -> (s, appSubsTy subst t)) vars

composeSubsTy :: SubsTy -> SubsTy -> SubsTy
composeSubsTy s1@(SubsTy l1) s2@(SubsTy l2) = SubsTy $ zip tsymbs types
    where
        tsymbs = nub $ (fst $ unzip l1) ++ (fst $ unzip l2)
        types = map ((appSubsTy s1) . (appSubsTy s2) . TVar) tsymbs

instance Monoid SubsTy where
    mempty = SubsTy []
    mappend = composeSubsTy

unify :: (MonadError String m) => Type -> Type -> m SubsTy
unify t1@(TVar _) t2@(TVar _) | t1 == t2  = return $ SubsTy []
unify BoolT BoolT = return $ SubsTy []
-- unify BoolT t = unify t BoolT
unify tv@(TVar ts) t = if elem ts $ freeTVars t then throwError $ "Can't unify " ++ show tv ++ " with " ++ show t ++ "!"
                                                else return $ SubsTy [(ts, t)]
unify t1@(_ :-> _) t2@(TVar _) = unify t2 t1
unify (s1 :-> s2) (t1 :-> t2) = do
    u2 <- unify s2 t2
    u1 <- unify (appSubsTy u2  s1) (appSubsTy u2 t1)
    return $ composeSubsTy u1 u2

unifyEqs :: (MonadError String m) => [(Type, Type)] -> m SubsTy
unifyEqs eqs = let (lt, rt) =  unzip eqs in unify (foldl1 (:->) lt) (foldl1 (:->) rt)

getFreshTVar :: [Symb] -> Symb
getFreshTVar [] = "t"
getFreshTVar ss = let c = 1 + (maximum $ map getNumber ss) in 't':(show c)
    where
        getNumber :: String -> Integer
        getNumber ('t':[]) = 0
        getNumber ('t':number) = read number

equations :: (MonadError String m) => Env -> Expr -> Type -> m [(Type, Type)]
equations env (Var x) sigma = do
    t <- appEnv env x
    return [(sigma, t)]

equations env (x :@ y) sigma = do
    let alpha = getFreshTVar $ freeTVars sigma `union` freeTVarsEnv env
    xeq <- equations env x (TVar alpha :-> sigma)
    yeq <- equations env y $ TVar alpha
    return $ xeq ++ yeq

equations env (Lam x y) sigma = do
    let alpha = getFreshTVar $ freeTVars sigma `union` freeTVarsEnv env
    let beta = getFreshTVar $ alpha : freeTVars sigma `union` freeTVarsEnv env
    yeq <- equations (extendEnv env x $ TVar alpha) y $ TVar beta
    return $ (TVar alpha :-> TVar beta, sigma):yeq

equations env TrueV sigma = return [(sigma, BoolT)]

equations env FalseV sigma = return [(sigma, BoolT)]

generateContext :: [Symb] -> [Symb] -> Env
generateContext [] _ = Env []
generateContext (t:ts) used = let ftv = getFreshTVar used
                              in extendEnv (generateContext ts $ ftv:used) t $ TVar ftv

principlePair :: (MonadError String m) => Expr -> m (Env, Type)
principlePair expr = do
    let sigma = getFreshTVar []
    let fvs = freeVars expr
    let env = generateContext fvs [sigma]
    eqs <- equations env expr $ TVar sigma
    subst <- unifyEqs eqs
    return (appSubsEnv subst env, appSubsTy subst $ TVar sigma)
