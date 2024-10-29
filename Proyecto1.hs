--paratodo :: [Bool] -> Bool, que verifica que todos los elementos de una lista sean True.

paratodo :: [Bool] -> Bool
paratodo [] = True
paratodo (x:xs) = x == True && paratodo xs