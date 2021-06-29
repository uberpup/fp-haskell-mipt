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

-- task 25
foldr1' :: (a -> a -> a) -> [a] -> a
foldr1' f xs = foldr f (last xs) (init xs)

-- task 26
lmax' :: Ord a => [a] -> a
lmax' = foldr1' (\x acc -> if x >= acc then x else acc)

-- task 27
scanl :: (b -> a -> b) -> b -> [a] -> [b]
scanl f z xs = foldr go (const []) xs z
  where
    go x continue acc = let next = f acc x in next : continue next

-- task 28
allPrefixes :: [a] -> [[a]]
allPrefixes (x:xs) = Main.scanl (\acc x -> acc ++ [x]) [x] xs

-- task 29
rot :: [a] -> [a]
rot xs = (last xs) : (init xs)

rotts' :: [a] -> [[a]]
rotts' xs = take (length xs) (iterate rot xs)

rotts'' :: [a] -> [[a]]
rotts'' xs = init $ Main.scanl (\ys _ -> rot ys) xs xs

-- task 30
unzip' :: [(a, b)] -> ([a], [b])
unzip' [] = ([], [])
unzip' xs = (map fst xs, map snd xs)

unzip'' :: [(a, b)] -> ([a], [b])
unzip'' = foldr (\(x,y) ~(xs, ys) -> (x:xs, y:ys)) ([], [])

-- task 31

-- f = curry g, g is a function that accepts a pair
-- g = uncurry f, f is a function that accepts two arguments
-- f x y = g (x, y)
curry' :: ((x, y) -> z) -> (x -> y -> z)
curry' f = \x y -> f (x, y)

uncurry' :: (x -> y -> z) -> ((x, y) -> z)
uncurry' f = \(x, y) -> f x y

-- task 32
-- f = curry $ h . (uncurry g)

-- task 33
zapp :: [a -> b] -> [a] -> [b]
zapp = curry ((map (uncurry ($))) . (uncurry zip))

-- task 34
-- [x,x,y,z,z,z] -> [(x, 2), (y, 1), (z, 3)]
-- Failed to implement using foldr, so ...
compress :: Eq a => [a] -> [(a, Int)]
compress xs = compressList $ transformList(xs)

compressList :: Eq a => [(a, Int)] -> [(a, Int)]
compressList [] = []
compressList (x:xs) | null xs = [x]
					| fst x == fst(head (xs)) = compressList((fst x, snd x + snd(head(xs))) : tail xs)   
					| otherwise = x : compressList xs

transformList :: Eq a => [a] -> [(a, Int)]
transformList xs = zip(xs) (repeat(1))

decompress :: [(a, Int)] -> [a]
decompress [] = []
decompress (x:xs) = [fst x | _ <- [1..snd(x)]] ++ decompress xs
