module Tests where

import TypeInference
import Type

term1 :: Expr
term1 = EVar "x"

env1 :: TypeEnv
env1 = fromListTypeEnv [("x", Scheme [] TInt)]

main :: IO ()
main = do
    print term1
