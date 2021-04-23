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