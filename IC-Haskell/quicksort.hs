-- Função que retorna todos os valores da lista menor que o pivô
menorQue :: Int -> [Int] -> [Int]
menorQue v [] = []
menorQue v (x:xs)
        |x<=v = x:menorQue v xs
        |otherwise = menorQue v xs

-- Função que retorna todos os valores da lista maior que o pivô
maiorQue :: Int -> [Int] -> [Int]
maiorQue v [] = []
maiorQue v (x:xs)
        |x>v = x:maiorQue v xs
        |otherwise = maiorQue v xs

-- Algorítmo de ordenação Quick Sort 
-- qsort [9,1,5,3,7,6,4,8,2,0]
qsort :: [Int] -> [Int]
qsort [] = []
qsort (x:xs) = qsort (menorQue x xs) ++ x:qsort (maiorQue x xs)