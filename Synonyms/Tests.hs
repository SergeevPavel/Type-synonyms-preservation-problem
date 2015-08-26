module Tests where

import TypeInference
import Type

test1 = ( EVar "x"
        , fromListTypeEnv [ ("x", Scheme [] TInt) ]
        )

test2 = ( EAbs "x" $ EApp (EApp (EVar "and") (EVar "x")) $ EVar "x"
        , fromListTypeEnv [ ("and", Scheme [] $ TBool :-> (TBool :-> TBool)) ]
        )


test3 = ( EApp (EVar "reverse") (EVar "str")
        , fromListTypeEnv [ ("reverse", Scheme ["a"] $ (TList $ TVar "a") :-> (TList $ TVar "a"))
                          , ("str", Scheme [] $ TSynonym "String" (TList TChar))
                          ]
        )

test4 = ( EApp (EApp (EVar "compose") (EVar "neg1")) (EVar "neg2")
        , fromListTypeEnv [ ("compose", Scheme ["a", "b", "c"] $ (TVar "b" :-> TVar "c") :-> (TVar "a" :-> TVar "b") :-> TVar "a" :-> TVar "c")
                          , ("neg1", Scheme [] $ (TSynonym "BinFun" TBool :-> TBool :-> TBool) :-> (TSynonym "BinFun" TBool :-> TBool :-> TBool))
                          , ("neg2", Scheme ["a", "b"] $ (TVar "a" :-> TVar "b" :-> TBool) :-> (TVar "a" :-> TVar "b" :-> TBool))
                          ]
        )

runTest :: (Expr, TypeEnv) -> IO ()
runTest (expr, env) = case typeInference env expr of
                        Left err      -> putStrLn $ "error: " ++ err
                        Right (env, ty) -> putStrLn $ (show expr)
                                                    ++ " :: "
                                                    ++ (show ty)
                                                    ++ " in "
                                                    ++ (show env)

main :: IO ()
main = do
    mapM_ runTest [ test1
                  , test2
                  , test3
                  , test4
                  ]
