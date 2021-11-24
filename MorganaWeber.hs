--Morgana Weber - Programação Funcional - Turma 010

{-
    1)  Escreva os tipos mais gerais para as seguintes funções:

    segundo xs = head (tail xs)
    tripla x = (x,x,x)
    soma x y = x + y
    pares = filter even
    aplica f x = f x 

segundo :: [A] -> A
tripla  :: A -> [A]
soma    :: A -> A -> A --Considerando que A pode ser um Int ou Double
pares   :: [Int] -> [Int]
aplica  :: (A->B) -> A -> B-}

{-
    2) Defina as funções abaixo usando recursão

    inc – soma 1 a todos elementos de uma lista de inteiros
    fac – retorna o fatorial de um número natural
    pares – retorna os elementos pares de uma lista de inteiros
    ordenado – decide se uma lista de inteiros está ordenada
-}
inc :: [Int] -> [Int]
inc []          = []
inc (x:xs)      = (x+1) : (inc xs)


fac :: Int -> Int
fac n   | n == 0    = 1
        | n > 0     = fac (n-1) * n 


pares :: [Int] -> [Int]
pares []                          = []
pares (x:xs)  | (mod x 2 == 0)    = x : pares xs
              | otherwise         = pares xs


ordenado :: [Int] -> Bool
ordenado []                   = True
ordenado [x]                  = True
ordenado (x:y:xs) | x > y     = False
                  | otherwise = ordenado (y:xs)

{-
    3) Dados dois números naturais, o algoritmo de Euclides subtrai o menor elemento do maior sucessivas vezes até que estes sejam iguais. O resultado é o máximo divisor comum entre os números originais.

    Defina a função euclides :: Int -> Int -> Int que implementa este algoritmo.
-}
euclides :: Int -> Int -> Int
euclides x y | x > y     = euclides y (x-y)
             | x < y     = euclides x (y-x)
             | otherwise = x            

{-
    4) Defina a função remover :: Int -> [Int] -> [Int] que remove a primeira ocorrência (se houver) de um inteiro de uma lista de inteiros. Por exemplo, remover 2 [1,2,3,4,2] deve retornar [1,3,4,2]
-}
remover :: Int -> [Int] -> [Int] 
remover y []                 = y : []
remover y (x:xs) | x == y    = xs
                 | otherwise = x:(remover y xs)

{-
    5) Através da função remover e da função padrão minimum :: [Int] -> Int, defina uma função recursiva ordena :: [Int] -> [Int] que ordena uma lista de inteiros a partir da seleção e remoção do menor elemento.
-}

ordena :: [Int] -> [Int]
ordena [] = []
ordena xs = minimum xs : ordena (remover (minimum xs) xs)
