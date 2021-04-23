-- task 35
fib :: Nat -> Nat
fib 0 = 0
fib 1 = 1
fib k = fib(k - 1) + fib(k - 2)

foldn :: (a -> a) -> a -> Nat -> a
foldn f x Z = x
foldn f x (S n) = f (foldn f x n)

fib' :: Nat -> Nat
