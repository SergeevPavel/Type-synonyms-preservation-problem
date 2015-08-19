module Substitution where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Type

newtype Subst = Subst (Map.Map String Type)

nullSubst :: Subst
nullSubst = Subst Map.empty

composeSubst :: Subst -> Subst -> Subst
composeSubst su1@(Subst s1) (Subst s2) = Subst $ (Map.map (apply su1) s2) `Map.union` s1

removeFromSubst :: String -> Subst -> Subst
removeFromSubst n (Subst s) = Subst $ Map.delete n s

singletonSubst :: String -> Type -> Subst
singletonSubst s t = Subst $ Map.singleton s t

instance Monoid Subst where
    mempty = nullSubst
    mappend = composeSubst

class Types a where
    ftv   :: a -> Set.Set String
    apply :: Subst -> a -> a

instance Types a => Types [a] where
    ftv l = foldr Set.union Set.empty (map ftv l)
    apply su = map $ apply su

instance Types Type where
    ftv (TVar id)       = Set.singleton id
    ftv (ty1 :-> ty2)   = ftv ty1 `Set.union` ftv ty2
    ftv (TInt)          = Set.empty
    ftv (TBool)         = Set.empty
    ftv (TList ty)      = ftv ty
    ftv (TPair ty1 ty2) = ftv ty1 `Set.union` ftv ty2
    ftv (TSynonym _ t)  = Set.empty

    apply (Subst s) (TVar id) = case Map.lookup id s of
                                                Nothing -> TVar id
                                                Just t  -> t
    apply su (t1 :-> t2)      = apply su t1 :-> apply su t2
    apply _  TInt             = TInt
    apply _  TBool            = TBool
    apply su (TList t)        = TList $ apply su t
    apply su (TPair t1 t2)    = TPair (apply su t1) (apply su t2)
    apply su ts@(TSynonym _ t)   = ts
