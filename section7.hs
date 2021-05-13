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

--