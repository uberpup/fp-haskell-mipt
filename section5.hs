-- task 9
head' :: [a] -> a
head' (x:xs) = x

tail' :: [a] -> [a]
tail' [] = []
tail' (x:xs) = xs

take' :: Int -> [a] -> [a]
take' _ [] = []
take' 0 (x:xs) = []
take' n (x:xs) = x : take' (n-1) (xs)

drop' :: Int -> [a] -> [a]
drop' 0 (x:xs) = (x:xs)
drop' 1 (x:xs) = xs
drop' n (x:xs) = drop' (n-1) (xs)

null' :: [a] -> Bool
null' [] = True
null' (x:xs) = False

elem' :: Eq a => a -> [a] -> Bool
elem' x [] = False
elem' x (y:ys) | x == y = True
			   | otherwise = elem' x (ys) 

nth' :: Int -> [a] -> a	 -- n less than [] length
nth' 0 (x:xs) = x
nth' n (x:xs) = nth' (n - 1) xs

listcon' :: [a] -> [a] -> [a]
listcon' [] (x:xs) = (x:xs)
listcon' (x:xs) [] = (x:xs)
listcon' (x:xs) ys = x : (listcon' xs ys)

concat' :: [[a]] -> [a]
concat' ([]:xs) = concat' xs
concat' ((x:xs):ys) = x : concat' (xs:ys)

-- task 10
segs :: [a] -> [[a]]
segs [] = [[]]
segs (x:xs) = [x : segs | segs <- segs xs] ++ segs xs

-- task 11

-- task 12

-- task 13

-- task 14
dupl :: [a] -> [a]
dupl [] = []
dupl (x:xs) = concatMap (replicate 2) (x:xs)

-- task 16
rmdupl :: Eq a => [a] -> [a]
rmdupl [] = []
rmdupl (x:xs)	| elem x xs = rmdupl xs
				| otherwise = x : rmdupl xs
-- task 21
