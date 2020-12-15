module Part3 where

------------------------------------------------------------
-- PROBLEM #18
--
-- Проверить, является ли число N простым (1 <= N <= 10^9)
prob18 :: Integer -> Bool
prob18 1 = False
prob18 n = all check [2..n `div` 2]
            where check x = n `mod` x /= 0

------------------------------------------------------------
-- PROBLEM #19
--
-- Вернуть список всех простых делителей и их степеней в
-- разложении числа N (1 <= N <= 10^9). Простые делители
-- должны быть расположены по возрастанию
sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (x:xs) = x : (sieve $ filter (\ y -> (y `mod` x) /= 0) xs )

factorize :: Integer -> [Integer]
factorize n = filter (\ x -> (n `mod` x)==0) $ sieve [2,3..n `div` 2]

getPow :: Integer -> Integer -> Integer -> Int
getPow n f ff | (n `mod` ff) /= 0 = 0
           | otherwise = 1 + getPow n  f (f *ff)

prob19 :: Integer -> [(Integer, Int)]
prob19 1 = []
prob19 n = if (map (\ f -> (f, getPow n f f)) $ factorize n) == [] then [(n, 1)] else map (\ f -> (f, getPow n f f)) $ factorize n

------------------------------------------------------------
-- PROBLEM #20
--
-- Проверить, является ли число N совершенным (1<=N<=10^10)
-- Совершенное число равно сумме своих делителей (меньших
-- самого числа)
divList'' :: Integer -> Integer -> [Integer]
divList'' n k | (k > (n `div` 2)) = []
             | (n `mod` k) == 0 = k : divList'' n (k+1)
             | otherwise =  divList'' n (k+1)

divList :: Integer -> [Integer]
divList n = divList'' n 1
prob20 :: Integer -> Bool
prob20 n = (n == (foldl (+) 0 (divList n)))

------------------------------------------------------------
-- PROBLEM #21
--
-- Вернуть список всех делителей числа N (1<=N<=10^10) в
-- порядке возрастания
divList' :: Integer -> Integer -> [Integer]
divList' n k | (k > (n `div` 2)) = [n]
             | (n `mod` k) == 0 = k : divList' n (k+1)
             | otherwise =  divList' n (k+1)

prob21 :: Integer -> [Integer]
prob21 n = divList' n 1

------------------------------------------------------------
-- PROBLEM #22
--
-- Подсчитать произведение количеств букв i в словах из
-- заданной строки (списка символов)

prob22 :: String -> Integer
prob22 n = foldl1 (*) (map toInteger (map (length) (map (filter (=='i'))(words n))))

------------------------------------------------------------
-- PROBLEM #23
--
-- На вход подаётся строка вида "N-M: W", где N и M - целые
-- числа, а W - строка. Вернуть символы с N-го по M-й из W,
-- если и N и M не больше длины строки. Гарантируется, что
-- M > 0 и N > 0. Если M > N, то вернуть символы из W в
-- обратном порядке. Нумерация символов с единицы.
prob23 :: String -> Maybe String
prob23 = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #24
--
-- Проверить, что число N - треугольное, т.е. его можно
-- представить как сумму чисел от 1 до какого-то K
-- (1 <= N <= 10^10)

--число является треугольным, если представляет из себя идеальный квадрат
--идиотская проверка, если честно
triangular :: Integer -> Float
triangular x = sqrt((8.0*(fromInteger x)) + 1)

prob24 :: Integer -> Bool
prob24 1 = True
prob24 n = if (round(triangular n))^2 == 8*n + 1 then True
                                                 else False
------------------------------------------------------------
-- PROBLEM #25
--
-- Проверить, что запись числа является палиндромом (т.е.
-- читается одинаково слева направо и справа налево)
prob25 :: Integer -> Bool
prob25 x = reversal x == x
reversal :: Integral a => a-> a
reversal = go 0
  where go a 0 = a
        go a b = let (q,r) = b `quotRem` 10 in go (a*10 + r) q


------------------------------------------------------------
-- PROBLEM #26
--
-- Проверить, что два переданных числа - дружественные, т.е.
-- сумма делителей одного (без учёта самого числа) равна
-- другому, и наоборот
divisors :: Integer -> [Integer]
divisors d = filter ((== 0) . (mod d)) [1..d]
prob26 :: Integer -> Integer -> Bool
prob26 n k = if foldr (+) 0 (divisors n) == foldr (+) 0 (divisors k) then True
                                                                   else False

------------------------------------------------------------
-- PROBLEM #27
--
-- Найти в списке два числа, сумма которых равна заданному.
-- Длина списка не превосходит 500
prob27 :: Int -> [Int] -> Maybe(Int, Int)
prob27 = error "Implement me!"
--sumAll x = [a+b | (a,i) <- p, (b,j) <- p , i /= j]
--                       where p=zip x [1..]
--prob27 n k = if length (filter (==n) (sumAll k)) > 0 then
--                                                     else Nothing

------------------------------------------------------------
-- PROBLEM #28
--
-- Найти в списке четыре числа, сумма которых равна
-- заданному.
-- Длина списка не превосходит 500
prob28 :: Int -> [Int] -> Maybe (Int, Int, Int, Int)
prob28 = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #29
--
-- Найти наибольшее число-палиндром, которое является
-- произведением двух K-значных (1 <= K <= 3)
prob29 :: Int -> Int
prob29 n = let pal n = (let s = show n in s == reverse s) in maximum [k | i <- [1..3], j <- [i..3], let k = i * j, pal k]
------------------------------------------------------------
-- PROBLEM #30
--
-- Найти наименьшее треугольное число, у которого не меньше
-- заданного количества делителей
prob30 :: Int -> Integer
prob30 = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #31
--
-- Найти сумму всех пар различных дружественных чисел,
-- меньших заданного N (1 <= N <= 10000)
prob31 :: Int -> Int
prob31 = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #32
--
-- В функцию передаётся список достоинств монет и сумма.
-- Вернуть список всех способов набрать эту сумму монетами
-- указанного достоинства
-- Сумма не превосходит 100
prob32 :: [Int] -> Int -> [[Int]]
prob32 = error "Implement me!"
