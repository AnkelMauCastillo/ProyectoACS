sumadigit:: Integer -> Integer
sumadigit n = if n < 10 then 1 else sumadigit(n `div` 10) + 1

cuentadigitos n = if n >= 1 then cuentadigitos(n `div` 10) + 1 else n

digitos:: Integer -> Integer
digitos 0 = 0
digitos n = 1 + digitos ((div n 10))

mcd x y = if y == 0 then x else mcd y (mod x y)

euclides x 0 = x
euclides x y = euclides y (mod x y)

potencia :: Integer -> Integer -> Integer
potencia x 0 = 1
potencia x y = x * potencia x (y-1)

nand :: Bool-> Bool -> Bool
nand True True = False
nand _ _ = True
nor :: Bool -> Bool -> Bool
nor False False = True
nor _ _ = False
{-
Definición con Guarda 
La evaluación de las guardas es de arriba hacia abajo y
retorna el resultado de la primera rama cierta, la igualdad
va después de cada guarda.
-}
absoluto :: Int -> Int
absoluto n
  | n >= 0 = n
  | otherwise = -n

{-
cobro luz
-}

cobroluz n
 | n <= 150 = 0.912*n 
 | n <= 280 = 136.80+ (n-150)*1.111
 | n > 280 = 281.23 + (n-280)*3.248 
 | otherwise = -1

cfe_iva n = if n > 0  then (cobroluz n) * 0.16 + cobroluz n else -1

cfeIva n 
 | n > 0 = (cobroluz n) * 0.16 + cobroluz n
 | otherwise = (-1)


mcm a b = div (a*b) (euclides a b)

esPrimo:: Int -> Bool
esPrimo 0 = False -- Caso Base
esPrimo 1 = False -- Caso Base
esPrimo 2 = True
esPrimo x = not (divisible x (x - 1))




divisible:: Int-> Int -> Bool
divisible x 1 = False
divisible x n = if mod x n == 0 then True else divisible x (n - 1)

esPrimo2:: Int -> Bool
esPrimo2 1 = False -- Caso Base
esPrimo2 2 = True
esPrimo2 n = not (divisible (n - 1))
   where
    divisible :: Int -> Bool
    divisible 1 = False
    divisible x = mod n x == 0 || divisible(x-1)







 
