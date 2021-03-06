{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

-- task 1
a = ([([(0,\x -> x + 1)], '0')]) :: [([(Int, Int->Int)], Char)]
b = (\[(x,y)] -> ([y],[x])) :: [(Char, Int)] -> ([Int], [Char])
c = \x -> (x,x)
d = \x y -> x y
-- task 2
-- a. (a, b) -> (b, a)
-- b. (a, a -> b) -> b
-- c. (a -> b, a) -> b
-- d. a -> (a -> b) -> b
-- e. infinite type
-- task 3
and' :: Bool -> Bool -> Bool
and' False _ = False
and' True x = x
impl' :: Bool -> Bool -> Bool
impl' _ True = True
impl' True x = x
xor' :: Bool -> Bool -> Bool
xor' True True = False
xor' False False = False
xor' _ _ = True
maj' :: Bool -> Bool -> Bool -> Bool
maj' _ True True = True
maj' _ False False = False
maj' x True False = x
maj' x False True = x
-- task 4
replicate' = \n x -> [x | y <- [1..n]]
-- task 5
triples = [(x, y, z) | z <- [1..], x <- [1..z], y <- [1..z], x*x + y*y == z*z]
-- task 6
amicable :: Int -> Int -> Bool
amicable = \x y -> (sum([i | i <- [1..x-1], x `mod` i == 0]) == y)
-- task 7
triangular = [n*(n + 1)/2 | n <- [1..]]
-- task 8
type Btr = (Bool, Bool, Bool)
type Bfn = Btr -> Bool 
-- a) Ring structure
-- b) instance of Eq, Num, Show
add' :: Bfn -> Bfn -> Bfn
add' f g x = f x or g x 

mlt' :: Bfn -> Bfn -> Bfn
mlt' f g x = f x and g x

--eq' :: Bfn -> Bfn -> Bool
--eq' f g

null' :: Bfn
null' _ = False

id' :: Bfn
id' _ = True

inv' :: Bfn -> Bfn
inv f x = not . f x