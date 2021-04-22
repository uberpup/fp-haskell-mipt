-- task 23
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> f x : xs) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x xs -> if p x then x : xs else xs) []

and' :: [Bool] -> Bool
and' = foldr (&&) True

or' :: [Bool] -> Bool
or' = foldr (||) False

all' :: (a -> Bool) -> [a] -> Bool
all' p = foldr (&&) True . map p

any' :: (a -> Bool) -> [a] -> Bool
any' p = foldr (||) False . map p

-- task 24
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p [] = []
takeWhile' p (x:xs) | p x = x : takeWhile' p xs
					| otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p [] = []
dropWhile' p (x:xs) | p x = dropWhile' p xs
					| otherwise = x : xs 
-- task 30
unzip' :: [(a, b)] -> ([a], [b])
unzip' =

unzip'' :: [(a, b)] -> ([a], [b])
unzip'' = 

-- task 31

-- f = curry g, g is a function that accepts a pair
-- g = uncurry f, f is a function that accepts two arguments
-- f x y = g (x, y)
curry' :: ((x, y) -> z) -> (x -> y -> z)
curry' f = \x y -> f (x, y)

uncurry' :: (x -> y -> z) -> ((x, y) -> z)
uncurry' f = \(x, y) -> f x y
-- task 33
-- task 34