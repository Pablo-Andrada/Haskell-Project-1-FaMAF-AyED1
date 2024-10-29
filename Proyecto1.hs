--Laboratorio 1 Ejercicio básico de repaso para comenzar a trabajar en el laboratorio. Programá las siguientes funciones:

--1. esCero :: Int -> Bool, que verifica si un entero es igual a 0.
esCero :: Int -> Bool
esCero n = n == 0

--2. esPositivo :: Int -> Bool, que verifica si un entero es estrictamente mayor a 0.
esPositivo :: Int -> Bool
esPositivo n = n > 0
--3. esVocal :: Char -> Bool, que verifica si un caracter es una vocal en minúscula.
esVocal :: Char -> Bool
esVocal x = x == 'a' ||x == 'e' ||x == 'i' ||x == 'o' ||x == 'u'
--4. valorAbsoluto :: Int -> Int, que devuelve el valor absoluto de un entero ingresado.
valorAbsoluto :: Int -> Int
valorAbsoluto n | n >= 0 = n
                | n < 0 = -n 


--Laboratorio 2 Implementá en Haskell las funciones definidas en el ejercicio anterior. A continuación mostramos algunos ejemplos del uso de las funciones en ghci:
-- $> p a r a t o d o [ True , F a l s e , True ]
-- F a l s e
-- $> p a r a t o d o [ True , True ]
-- True
-- $> s um a t o r i a [ 1 , 5 , −4]
-- 2
-- $> p r


-- a) paratodo :: [Bool] -> Bool, que verifica que todos los elementos de una lista sean True.
paratodo :: [Bool] -> Bool
paratodo [] = True
paratodo (x:xs) = x == True && paratodo xs

-- b) sumatoria :: [Int] -> Int, que calcula la suma de todos los elementos de una lista de enteros.
sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

--c) productoria :: [Int] -> Int, que calcula el producto de todos los elementos de la lista de enteros.
productoria :: [Int] -> Int
productoria [] = 1
productoria (x:xs) = x * (productoria xs)

--d) factorial :: Int -> Int, que toma un n ́umero n y calcula n!.
factorial :: Int -> Int
factorial x      | x == 0 || x == 1 = 1
                 | otherwise = x* factorial (x-1)

--e) Utilizá la función sumatoria para definir, promedio :: [Int] -> Int, que toma una lista de números no vacia y calcula el valor promedio (truncado, usando división entera).

promedio :: [Int] -> Int
promedio xs = sumatoria xs `div` length xs