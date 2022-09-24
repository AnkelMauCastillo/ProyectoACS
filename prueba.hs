doble:: Int -> Int
doble x = 2*x

prectangulo :: Int -> Int -> Int
prectangulo base altura = doble(base + altura)

factorial :: Integer -> Integer
factorial n = if n==0 then 1 else n * factorial(n-1)


factorialr 0 = 1
factorialr n = n * factorial(n-1)
