module Examples where


-- Example 1. Lists
-- Тип выводится по первому элементу
type TS1 = Char
type TS2 = Char

lst1 = ['a' :: TS1, 'b' :: TS2, 'c']
-- :t lst1 :: [TS1]
--
lst2 = ['a' :: TS2, 'b' :: TS1, 'c']
-- :t lst2 :: [TS2]
--
lst3 = ['a', 'b' :: TS1, 'c' :: TS2]
-- :t lst3 :: [Char]

-- Example 2. BinFun
--
type BinFun = Bool -> Bool -> Bool
neg = (\ f x y -> not (f x y)) :: BinFun -> BinFun
neg' = \ f x y -> not (f x y) -- (a -> b -> Bool) -> (a -> b -> Bool)
pass = neg' . neg -- BinFun -> Bool -> Bool -> Bool

-- Example 3. Multiple synonymous
--
foo = (\ x y z -> x) :: a -> a -> a -> a
bar = foo ('x' :: TS1) ('x' :: TS2) 'x' -- TS1
bar' = foo 'x' ('x' :: TS1) ('x' :: TS2) -- Char


-- Example 4.
--
a :: String
a = "abc"

b :: String
b = "def"

strConcat :: String -> String -> String
strConcat = (++)

addABC = (++) ("ABC" :: String) -- [Char] -> [Char]

cba = reverse ("abc" :: String) -- [Char]
