import System.IO
main = do
         putStr "Ingresa Factorial"
         hFlush stdout
         num <- getLine
         print(factorial2 (read num::Integer))
factorial2::Integer->Integer
factorial2 n = if n == 0 then
               1
               else
                n*factorial2(n-1)
