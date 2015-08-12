module Tests where

import TypeInference

term1 :: Term
term1 = Term (EVar "x") Nothing Nothing

main :: IO ()
main = do
    print term1
