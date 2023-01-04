1
mdc :: Int -> Int -> Int
mdc a b
  |a == b = a
  |a > b = mdc (a-b) b
  |otherwise = mdc a (b-a)

2
mdc :: Int -> Int -> Int
mdc a b
  |a == b = a
  |a > b = mdc (a-b) b
  |otherwise = mdc a (b-a)

mmc :: Int -> Int -> Int
mmc _ 0 =  0
mmc 0 _ =  0
mmc x y =  abs ((x `quot` mdc x y) * y)

3
isPrimo :: Int -> Bool
isPrimo 1 = False
isPrimo 2 = True
isPrimo n
  |mod n 2 == 0 = False
  |otherwise = verifica 3 n

verifica :: Int -> Int -> Bool
verifica i n
  |i == n = True
  |mod n i == 0 = False
  |otherwise = verifica (i+1) n

primos :: [Int] -> [Int]
primos [] = []
primos (x:xs)
  |isPrimo x == True = x:primos xs
  |otherwise = primos xs

4
isPrimo :: Int -> Bool
isPrimo 1 = False
isPrimo 2 = True
isPrimo n
  |mod n 2 == 0 = False
  |otherwise = verifica 3 n

verifica :: Int -> Int -> Bool
verifica i n
  |i == n = True
  |mod n i == 0 = False
  |otherwise = verifica (i+1) n

naoPrimos :: [Int] -> [Int]
naoPrimos [] = []
naoPrimos (x:xs)
  |isPrimo x == False = x:naoPrimos xs
  |otherwise = naoPrimos xs

5
isPrimo :: Int -> Bool
isPrimo 1 = False
isPrimo 2 = True
isPrimo n
  |mod n 2 == 0 = False
  |otherwise = verifica 3 n

verifica :: Int -> Int -> Bool
verifica i n
  |i == n = True
  |mod n i == 0 = False
  |otherwise = verifica (i+1) n

contaPrimos :: [Int] -> Int
contaPrimos [] = 0
contaPrimos (x:xs)
  |isPrimo x == True = 1 + contaPrimos xs
  |otherwise = contaPrimos xs

6
divide :: [Int] -> ([Int], [Int])
divide (x:xs) = (listaDiv3 (x:xs), lista2(x:xs))


div3 :: Int -> Bool
div3 x
  |mod x 3 == 0 = True
  |otherwise = False

listaDiv3 :: [Int] -> [Int]
listaDiv3 [] = []
listaDiv3 (x:xs)
  |div3 x == True = x : listaDiv3 xs
  |otherwise = listaDiv3 xs

div5 :: Int -> Bool
div5 x
  |mod x 5 == 0 = True
  |otherwise = False

lista2 :: [Int] -> [Int]
lista2 [] = []
lista2 (x:xs)
  |div3 x == False && div5 x == False = x : lista2 xs
  |otherwise = lista2 xs

7
maiormenor :: [Int] -> [Int]
maiormenor [] = []
maiormenor (x:xs) = [maior (x:xs), menor (x:xs)]


maior :: [Int] -> Int
maior [] = 0
maior [x] = x
maior (x:y:xs)
  |x > y = maior (x:xs)
  |otherwise = maior (y:xs)

menor :: [Int] -> Int
menor [] = 0
menor [x] = x
menor (x:y:xs)
  |x < y = menor (x:xs)
  |otherwise = menor (y:xs)

8
mapeia :: (Int -> Int) -> [Int] -> [Int]
mapeia f [] = []
mapeia f (x:xs) = f x : mapeia f xs

9
filtro :: (Int -> Bool) -> [Int] -> [Int]
filtro f [] = []
filtro f (x:xs)
  |f x = x : filtro f xs
  |otherwise = filtro f xs

10
divlista :: (Int -> Bool) -> [Int] -> ([Int],[Int])
divlista f [] = ([],[])
divlista f (x:xs) = (listaV f (x:xs), listaF f (x:xs))


listaV :: (Int -> Bool) -> [Int] -> [Int]
listaV f [] = []
listaV f (x:xs)
  |f x = x : listaV f xs
  |otherwise = listaV f xs

listaF :: (Int -> Bool) -> [Int] -> [Int]
listaF f [] = []
listaF f (x:xs)
  |not(f x) = x: listaF f xs
  |otherwise = listaF f xs

11
qualquer :: (Int -> Bool) -> [Int] -> Bool
qualquer f (x:xs)
  |f x == False = False
  |f x == True = True
  |f x == True = qualquer f xs
  |otherwise = qualquer f xs

12
todos :: (Int -> Bool) -> [Int] -> Bool
todos f (x:xs)
  |f x == False = False
  |f x == True = True
  |f x == True = todos f xs
  |otherwise = todos f xs


par :: Int -> Bool
par x
  |even x = True
  |otherwise = False

impar :: Int -> Bool
impar x
  |odd x = True
  |otherwise = False

13
qsatisfaz :: (Int -> Bool) -> [Int] -> Int
qsatisfaz f [] = 0
qsatisfaz f (x:xs)
  |f x = 1 + qsatisfaz f xs
  |otherwise = qsatisfaz f xs

par :: Int -> Bool
par x
  |even x = True
  |otherwise = False

impar :: Int -> Bool
impar x
  |odd x = True
  |otherwise = False

14
menores :: Int -> [Int] -> [Int]
menores x [] = []
menores x (y:xs)
  |x >= y = y : menores x xs
  |otherwise = menores x xs

maiores :: Int -> [Int] -> [Int]
maiores x [] = []
maiores x (y:xs)
  |x < y = y : maiores x xs
  |otherwise = maiores x xs

particionar :: Int -> [Int] -> ([Int],[Int])
particionar x (y:xs) = (menores x (y:xs), maiores x (y:xs))

15
menorQue :: Int -> [Int] -> [Int]
menorQue v [] = []
menorQue v (x:xs)
        |x<=v = x:menorQue v xs
        |otherwise = menorQue v xs

maiorQue :: Int -> [Int] -> [Int]
maiorQue v [] = []
maiorQue v (x:xs)
        |x>v = x:maiorQue v xs
        |otherwise = maiorQue v xs

qsort :: [Int] -> [Int]
qsort [] = []
qsort (x:xs) = qsort (menorQue x xs) ++ x:qsort (maiorQue x xs)

concatL1L2 :: [Int] -> [Int] -> [Int]
concatL1L2 x y = x ++ y

intercala :: [Int] -> [Int] -> [Int]
intercala x y = qsort (concatL1L2 x y)