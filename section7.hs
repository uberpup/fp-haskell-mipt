data Nat = Z | S Nat
        deriving (Eq,Ord,Show)

foldn :: (a -> a) -> a -> Nat -> a
foldn f x Z = x
foldn f x (S n) = f (foldn f x n)


nadd :: Nat -> Nat -> Nat
nadd m Z  = m
nadd m (S n) = S (nadd n m)

npred = snd . foldn (\(n,x) -> (S n, n)) (Z,Z)

-- task 35
fib :: Nat -> Nat
fib Z = Z
fib (S Z) = (S Z)
fib (S (S k))   = nadd (fib k) (fib(S k))

--fib' :: Nat -> Nat
--fib' k = foldn 

-- task 37
--

data Tree a = Nil | Node (Tree a) a (Tree a)
            deriving (Eq,Show)


root :: Tree a -> Maybe a            
root Nil = Nothing
root (Node _ x _) = Just x

flatten :: Tree a -> [a]
flatten Nil = []
flatten (Node l x r) = flatten l ++ [x] ++ flatten r

myTree2 = Node (Node (Node (Node (Node Nil 0 Nil) 1 Nil) 2 Nil ) 3 Nil) 4
        (Node Nil 5 (Node Nil 6 (Node Nil 7 (Node Nil 8 Nil))))

treeSum  :: Tree Integer -> Integer  -- Function for task 37
treeSum Nil = 0
treeSum (Node l x r) = sum $ flatten (Node l x r)
                      
-- task 38

foldt :: (b -> a -> b -> b) -> b -> Tree a -> b
foldt f x Nil = x
foldt f x (Node l y r) = f (foldt f x l) y (foldt f x r)

mapt :: (a -> b) -> Tree a -> Tree b
mapt g = foldt (\l x r -> Node l (g x) r) Nil

----------------

height :: Tree a -> Int
height = foldt (\x _ y -> 1 + max x y) 0

-- balanced in AVL style
isBlncd :: Tree a -> Bool
isBlncd Nil = True
isBlncd (Node l _ r) = isBlncd l && isBlncd r && abs(height l - height r) <= 1

left :: Tree a -> Tree a
left Nil = Nil
left (Node l x r) = l

right :: Tree a -> Tree a
right Nil = Nil
right (Node l x r) = r

value :: (Ord a, Num a) => Tree a -> a
value Nil = 0
value (Node l x r) = x

insAVL :: (Ord a, Num a) => a -> Tree a -> Tree a -- Function for tak 38
insAVL a Nil = Node Nil a Nil
insAVL a (Node l x r) | x < a = balanceAVL (Node l x (insAVL a r))
                      | otherwise = balanceAVL (Node (insAVL a l) x r)

balanceAVL :: (Ord a, Num a) => Tree a -> Tree a
balanceAVL Nil = Nil
balanceAVL (Node l x r) | not (isBlncd l) = Node (balanceAVL l) x r
                        | not (isBlncd r) = Node l x (balanceAVL r)
                        | (height l) + 1 < (height r) &&
                          (height (left r)) < (height (right r)) =
                           Node (Node l x (left r)) (value r) (right r)
                        | (height r) + 1 < (height l) &&
                          (height (right l)) < (height (left l)) =
                           Node (left l) (value l) (Node (right l) x r)
                        | (height l) + 1 < (height r) &&
                          (height (left r)) > height (right r) = 
                           Node (Node l x (left (left r))) (value (left r)) (Node (right (left r)) (value r) (right r))
                        | (height r) + 1 < (height l) &&
                          (height (right l)) > (height (left l)) = 
                           Node (Node (left l) (value l) (left (right l))) (value (right l)) (Node (right (right l)) x r)
                        | otherwise = Node l x r