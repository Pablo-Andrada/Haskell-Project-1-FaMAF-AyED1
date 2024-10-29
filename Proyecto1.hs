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

--d) factorial :: Int -> Int, que toma un número n y calcula n!.
factorial :: Int -> Int
factorial x      | x == 0 || x == 1 = 1
                 | otherwise = x* factorial (x-1)

--e) Utilizá la función sumatoria para definir, promedio :: [Int] -> Int, que toma una lista de números no vacia y calcula el valor promedio (truncado, usando división entera).

promedio :: [Int] -> Int
promedio xs = sumatoria xs `div` length xs

-- Laboratorio 3 A partir de las expresiones de los ejercicios 4a, 4b y 4d
--a) Identificá las variables libres de cada expresión y el tipo de cada una.
-- Ya identificamos que:

--xs y ys son listas [Int]
--x es un entero Int.

--b) Definí funciones que tomen como argumento las variables libres identificadas y devuelvan el resultado de la expresión. Atención: Tené en cuenta que en algunos casos es necesario definir funciones auxiliares.

----4a)Verificar si todos los elementos de xs son mayores a 0
todosMayoresACero :: [Int] -> Bool
todosMayoresACero [] = True
todosMayoresACero (x:xs) = x > 0 && todosMayoresACero xs

----4.b) Verificar si algún elemento de xs es igual a x
existeIgualA :: Int -> [Int] -> Bool
existeIgualA _ [] = False
existeIgualA x (y:ys) = x == y || existeIgualA x ys

----4.c) Verificar si todos los elementos de xs están en ys
todosEnYs :: [Int] -> [Int] -> Bool
todosEnYs [] _ = True
todosEnYs (x:xs) ys = elem x ys && todosEnYs xs ys

----4.d) Verificar si todos los elementos de xs son iguales entre sí
todosIguales :: [Int] -> Bool
todosIguales [] = True
todosIguales [_] = True
todosIguales (x:y:xs) = x == y && todosIguales (y:xs)


--c) Evaluá las funciones tomando como argumento los valores señalados en 5.

{- Para el Ejercicio 5, se proporcionan ejemplos específicos de listas para probar cada función:

1. xs = [-5, -3, 4, 8]
2. xs = [11, 2, 5, 8], con x = 5
3. ys = [2, -3, 11, 5, 8]

Evaluemos con estos valores:

4.a con xs = [-5, -3, 4, 8] y xs = [11, 2, 5, 8]:

todosMayoresACero [-5, -3, 4, 8] -- Resultado: False
todosMayoresACero [11, 2, 5, 8]  -- Resultado: True

4.b con x = 5 y xs = [11, 2, 5, 8]:
existeIgualA 5 [11, 2, 5, 8] -- Resultado: True

4.c con xs = [11, 2, 5, 8] y ys = [2, -3, 11, 5, 8]:
todosEnYs [11, 2, 5, 8] [2, -3, 11, 5, 8] -- Resultado: True

4.d con xs = [11, 2, 5, 8] y xs = [-5, -3, 4, 8]:
todosIguales [11, 2, 5, 8] -- Resultado: False
todosIguales [11, 11, 11, 11] -- Resultado: True


-}