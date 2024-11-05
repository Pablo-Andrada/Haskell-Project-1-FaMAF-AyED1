--Laboratorio 1 Ejercicio básico de repaso para comenzar a trabajar en el laboratorio. Programá las siguientes funciones:

--1. esCero :: Int -> Bool, que verifica si un entero es igual a 0.
esCero :: Int -> Bool
esCero n = n == 0

--2. esPositivo :: Int -> Bool, que verifica si un entero es estrictamente mayor a 0.
esPositivo :: Int -> Bool
esPositivo n = n >= 0
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

{-
Laboratorio 4 A partir de las expresiones en el ejercicio 7
a) Identific ́a las variables libres de cada expresi ́on y el tipo de cada una.
7.a) Producto de los enteros desde 1 hasta n
                ⟨∏i:1≤i≤n:i⟩
- Variables libres: n es un entero (Int)
- Significado: Calcula el producto de todos los números enteros desde 1 hasta n.

7.b) Promedio de los elementos de xs
                ⟨∑i:0≤i<#xs:xs.i⟩ / #xs
- Variables libres: xs es una lista de enteros [Int].
- Significado: Calcula el promedio (truncado a entero) de los elementos de la lista xs.​

7.c) Comparación entre el máximo de xs y el mínimo de ys
            ⟨Maxi:0≤i<#xs:xs.i⟩<⟨Mini:0≤i<#ys:ys.i⟩
- Variables libres: xs y ys son listas de enteros [Int].
- Significado: Verifica si el máximo valor en la lista xs es menor que el mínimo valor en la lista ys.

7.d) Existencia de índices i y j cuyo producto es n
                ⟨∃i,j:(2≤i<n)∧(2≤j<n):i×j=n⟩
- Variables libres: n es un entero (Int).
- Significado: Verifica si existen dos índices i y j entre 2 y n-1 tales que su producto es igual a n.
-}
--b) Definí funciones que tomen como argumento las variables libres identificadas y devuelvan el resultado de la expresión. Atención: Tené en cuenta que en algunos casos es necesario definir varias funciones.

--7.a) Producto de los enteros desde 1 hasta n
productoHastaN :: Int -> Int
productoHastaN n = product [1..n]

--7.b) Promedio de los elementos de xs
promedio' :: [Int] -> Int
promedio' xs = sum xs `div` length xs

--7.c) Comparación entre el máximo de xs y el mínimo de ys
maxMenorQueMin :: [Int] -> [Int] -> Bool
maxMenorQueMin xs ys = maximum xs < minimum ys

--7.d) Existencia de índices i y j cuyo producto es n
--existeProductoIgualA :: Int -> Bool
--existeProductoIgualA n = or [i * j == n | i <- [2..n-1], j <- [2..n-1]]

-- Función auxiliar que verifica si existe un `j` tal que `i * j == n`, con `j` en el rango `[2..n-1]`
existeMultiplicadorRec :: Int -> Int -> Int -> Bool
existeMultiplicadorRec i j n
  | j >= n    = False            -- Caso base: si `j` es igual o mayor que `n`, no encontramos solución
  | i * j == n = True            -- Caso en el que `i * j` es igual a `n`, encontramos solución
  | otherwise = existeMultiplicadorRec i (j + 1) n -- Continuamos con el siguiente `j`

-- Función principal que verifica si existe algún par `(i, j)` tal que `i * j == n`, con `i` en el rango `[2..n-1]`
existeProductoIgualARec :: Int -> Int -> Bool
existeProductoIgualARec i n
  | i >= n    = False                           -- Caso base: si `i` es igual o mayor que `n`, no encontramos solución
  | existeMultiplicadorRec i 2 n = True         -- Si encontramos un `j` tal que `i * j == n`, retornamos `True`
  | otherwise = existeProductoIgualARec (i + 1) n -- Continuamos con el siguiente `i`

-- Función para verificar si existe algún par `(i, j)` tal que `i * j == n`, comenzando con `i = 2`
existeProductoIgualA :: Int -> Bool
existeProductoIgualA n = existeProductoIgualARec 2 n

--c) Evaluá las funciones tomando como argumento los valores señalados en el ejercicio 8.
{- Ahora evaluaremos estas funciones con los valores dados en el Ejercicio 8:
  1-Para la función productoHastaN con n = 5:
    productoHastaN 5 -- Resultado: 120

  2-Para la función promedio con xs = [6, 9, 3, 9, 8]:
    promedio [6, 9, 3, 9, 8] -- Resultado: 7

  3-Para la función maxMenorQueMin con xs = [-3, 9, 8] y ys = [6, 7, 8]:
    maxMenorQueMin [-3, 9, 8] [6, 7, 8] -- Resultado: False
  
  4-Para la función existeProductoIgualA con n = 5:
    existeProductoIgualA 5 -- Resultado: False
-}

-- Definición de esPar
esPar :: Int -> Bool
esPar x = x `mod` 2 == 0

-- Definición de algunof usando el tipo genérico `a`
algunof :: (a -> Bool) -> [a] -> Bool
algunof _ [] = False
algunof f (x:xs) = f x || algunof f xs

-- Ejemplos de uso
-- Verifica si hay algún número par en la lista
-- resultado1 = algunof esPar [1, 2, 3, 4] -- True
-- resultado2 = algunof esPar [1, 3, 5]    -- False

--Laboratorio 5 Implementá en Haskell la función que definiste en el ejercicio anterior (Ejercicio 10).
todos:: [Bool] -> Bool
todos [] = True
todos (x:xs) = x == True && todos xs
-- Ejemplos de uso
--todos [False]         -- False
--todos [True,True]     -- True
--todos [True,False]    -- False

-- Laboratorio 6 A partir de las expresiones del ejercicio anterior b) y c y d)
{- a) Identificá las variables libres de cada expresión y el tipo de cada una.
b) Definí funciones que tomen como argumento las variables libres identificadas y devuelvan el resultado de la
expresión. Atención: Tené en cuenta que en algunos casos es necesario definir varias funciones.

Ejercicio 11b: n es el elemento más grande de xs
Parte a) Identificación de variables libres y tipos
Variables libres:

-n: entero (Int)
-xs: lista de enteros ([Int])
Explicación: Esta expresión verifica si el valor n es el máximo valor en la lista xs.

Parte b) Definición de función
Para verificar si n es el elemento más grande de xs, necesitamos comparar n con el máximo de xs.

-}
esMaximo :: Int -> [Int] -> Bool
esMaximo n xs = n == maximum xs

{-
Explicación:

Usamos la función maximum de Haskell para obtener el valor máximo de la lista xs.
Luego, comparamos n con este valor máximo. Si son iguales, n es el elemento más grande de xs.
-}
{-
Ejercicio 11c: Producto de los elementos pares de xs
Parte a) Identificación de variables libres y tipos
Variables libres:

xs: lista de enteros ([Int])
Explicación: Esta expresión calcula el producto de todos los elementos pares en la lista xs.

Parte b) Definición de función
Para calcular el producto de los elementos pares en xs, podemos filtrar los elementos pares y luego calcular su producto.
-}
-- Función que filtra los números pares de una lista
esPar' :: [Int] -> [Int]
esPar' xs = [x | x <- xs, even x]

productoPares :: [Int] -> Int
productoPares xs = product (esPar' xs)


--productoPares :: [Int] -> Int
--productoPares xs = ()

--productoPares :: [Int] -> Int
--productoPares xs = product [x | x <- xs, even x]


{-
Ejercicio 11d: Suma de los elementos en posición par de xs
Parte a) Identificación de variables libres y tipos
Variables libres:

xs: lista de enteros ([Int])
Explicación: Esta expresión calcula la suma de los elementos en xs que están en posiciones pares (0, 2, 4, ...).

Parte b) Definición de función
Para obtener la suma de los elementos en posición par, necesitamos tomar solo los elementos en índices pares y luego sumarlos.
-}
-- Función que toma los elementos en posiciones pares de una lista
elementosEnPosicionPar :: [Int] -> [Int]
elementosEnPosicionPar xs = [x | (x, i) <- zip xs [0..], even i]

-- Función que suma los elementos en posiciones pares de una lista usando elementosEnPosicionPar
sumaElementosEnPosicionPar :: [Int] -> Int
sumaElementosEnPosicionPar xs = sum (elementosEnPosicionPar xs)

--Laboratorio 7 Programá las funciones definidas en el ejercicio 27. Para definir los tipos en haskell lo pod ́es hacer de la siguiente manera:
--a) paratodo’ :: [a] -> (a -> Bool) -> Bool
--b) existe’ :: [a] -> (a -> Bool) -> Bool
--c) sumatoria’ :: [a] -> (a -> Int) -> Int
--d) productoria’ :: [a] -> (a -> Int) -> Int


--a) paratodo’ :: [a] -> (a -> Bool) -> Bool, dada una lista xs de tipo [a] y un predicado t :: a -> Bool, determina si todos los elementos de xs satisfacen el predicado t.
paratodo' :: [a] -> (a -> Bool) -> Bool
paratodo' [] _ = True
paratodo' (x:xs) predicado    | predicado x    = paratodo' xs predicado  
                              | otherwise = False 

-- *Main> paratodo' [5,4,4] esPositivo  
-- True
-- *Main> paratodo' [655,454,(-8)] esPositivo  
-- False



--b) existe’ :: [a] -> (a -> Bool) -> Bool, dada una lista xs de tipo [a] y un predicado t :: a -> Bool, determina si algún elemento de xs satisface el predicado t.
existe' :: [a] -> (a -> Bool) -> Bool
existe' [] _ = False  
existe' (x:xs) predicado
    | predicado x = True  
    | otherwise   = existe' xs predicado  

-- *Main> existe' [0,5,8,7] esCero 
-- True
-- *Main> existe' [5,8,7] esCero 
-- False


-- otra implementacion: 
-- existe' :: [a] -> (a -> Bool) -> Bool
-- existe'[] _ = False
-- existe' (x:xs) fn = fn x || existe' xs fn

--c) sumatoria’ :: [a] -> (a -> Int) -> Int, dada una lista xs de tipo [a] y una función t :: a -> Int (toma elementos de tipo a y devuelve enteros), calcula la suma de los valores que resultan de la aplicación de t a los elementos de xs.

sumatoria' :: [a] -> (a -> Int) -> Int
sumatoria' [] _ = 0 
sumatoria' (x:xs) funcion = funcion x + sumatoria' xs funcion

-- *Main> sumatoria' [1,3] factorial
-- 7
-- *Main> sumatoria' [4,5,0] factorial
-- 145


--d) productoria’ :: [a] -> (a -> Int) -> Int, dada una lista de xs de tipo [a] y una función t :: a -> Int, calcula el producto de los valores que resultan de la aplicación de t a los elementos de xs.
productoria' :: [a] -> (a -> Int) -> Int
productoria' [] _ = 1
productoria' (x:xs) predicado = predicado x * productoria' xs predicado

-- *Main> productoria' [2,(-2)] valorAbsoluto 
-- 4
-- *Main> productoria' [(-1515),(-2)] valorAbsoluto 
-- 3030

{-
Laboratorio 8 Teniendo en cuenta las funciones definidas en el Laboratorio 1 y en el Laboratorio 7, evalu ́a las
expresiones como se muestra en el ejemplo.

Ejemplos en ghci:
$> pa ra t o d o ’ [ 0 , 0 , 0 , 0 ] e sC e r o
True
$> pa ra t o d o ’ [ 0 , 0 , 1 , 0 ] e sC e r o
F a l s e
$> pa ra t o d o ’ ” h o l a ” e sV o c a l
F a l s e
$> e x i s t e ’ [ 0 , 0 , 1 , 0 ] e sC e r o
True
$> e x i s t e ’ ” h o l a ” e sV o c a l
True
$> e x i s t e ’ ” t n t ” e sV o c a l
F a l s e

---Funciones del Lab 7 y Lab 1, evaluadas:
ghci> paratodo' [0,0,3,0] esCero
False 
ghci> paratodo' [0,0,0,0] esCero
True 
ghci> paratodo' [0,87,3,10] esPositivo
True
ghci> paratodo' [(-1),87,3,10] esPositivo
False
ghci> paratodo' ['o','p','a'] esVocal
False
ghci> paratodo' ['o','e','a'] esVocal
True
ghci> existe' [0,1,2,3] esCero
True
ghci> existe' [4,1,2,3] esCero
False
ghci> existe' [4,1,2,3] esPositivo
True
ghci> existe' [(-3),1,2,3] esPositivo
True
ghci> existe' ['e','p','y','r'] esVocal
True
ghci> existe' ['w','p','y','r'] esVocal
False
ghci> sumatoria' [0,5,4,7] valorAbsoluto
16
ghci> sumatoria' [(-96),5,4,7] valorAbsoluto
112
ghci> productoria' [(-96),5,4,7] valorAbsoluto
13440
ghci> productoria' [(-96),5,4,0] valorAbsoluto
0
-}

