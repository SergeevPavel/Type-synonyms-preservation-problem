module Tests where

import TypeInference
import Type

test1 = ( EVar "x"
        , fromListTypeEnv [("x", Scheme [] TInt)]
        )

test2 = ( EAbs "x" $ EApp (EApp (EVar "and") (EVar "x")) $ EVar "x"
        , fromListTypeEnv [("and", Scheme [] $ TBool :-> (TBool :-> TBool))]
        )

runTest :: (Expr, TypeEnv) -> IO ()
runTest (expr, env) = case typeInference env expr of
                        Left error      -> putStrLn $ "error: " ++ error
                        Right (env, ty) -> putStrLn $ (show expr)
                                                    ++ " :: "
                                                    ++ (show ty)
                                                    ++ " in "
                                                    ++ (show env)

main :: IO ()
main = do
    mapM_ runTest [ test1
                  , test2
                  ]
