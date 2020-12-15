module Part2 where

import Part2.Types
import Data.Maybe
------------------------------------------------------------
-- PROBLEM #6
--
-- Написать функцию, которая преобразует значение типа
-- ColorLetter в символ, равный первой букве значения
prob6 :: ColorLetter -> Char
prob6 RED = 'R'
prob6 BLUE = 'B'
prob6 GREEN = 'G'

------------------------------------------------------------
-- PROBLEM #7
--
-- Написать функцию, которая проверяет, что значения
-- находятся в диапазоне от 0 до 255 (границы входят)
prob7 :: ColorPart -> Bool
prob7 (Red a) = if (a < 255) && (a > 0) then True else False
prob7 (Blue a) = if (a < 255) && (a > 0) then True else False
prob7 (Green a) = if (a < 255) && (a > 0) then True else False

------------------------------------------------------------
-- PROBLEM #8
--
-- Написать функцию, которая добавляет в соответствующее
-- поле значения Color значение из ColorPart
prob8 :: Color -> ColorPart -> Color
prob8 (Color x y z) (Red x1) = (Color (x+x1) y z )
prob8 (Color x y z) (Green y1) = (Color x (y+y1) z )
prob8 (Color x y z) (Blue z1) = (Color x y (z+z1) )

------------------------------------------------------------
-- PROBLEM #9
--
-- Написать функцию, которая возвращает значение из
-- ColorPart
prob9 :: ColorPart -> Int
prob9 (Red a) = a
prob9 (Blue a) = a
prob9 (Green a) = a

------------------------------------------------------------
-- PROBLEM #10
--
-- Написать функцию, которая возвращает компонент Color, у
-- которого наибольшее значение (если такой единственный)
prob10 :: Color -> Maybe ColorPart
prob10 (Color x y z) | x > y && x > z = Just (Red x)
                     | y > x && y > z = Just (Green x)
                     | z > y && z > y = Just (Blue x)
                     | otherwise      = Nothing

------------------------------------------------------------
-- PROBLEM #11
--
-- Найти сумму элементов дерева

prob11 :: Num a => Tree a -> a
prob11 (Tree l m r) = m + (if isJust l then (prob11 $ fromJust l) else 0) + (if isJust r then (prob11 $ fromJust r) else 0)


------------------------------------------------------------
-- PROBLEM #12
--
-- Проверить, что дерево является деревом поиска
-- (в дереве поиска для каждого узла выполняется, что все
-- элементы левого поддерева узла меньше элемента в узле,
-- а все элементы правого поддерева -- не меньше элемента
-- в узле)
getRoot (Tree l m r) = m
prob12 :: Ord a => Tree a -> Bool
prob12 (Tree l m r) | isJust l && isJust r       = if (getRoot (fromJust l)) < m && (getRoot (fromJust r)) > m then (prob12 (fromJust l) && prob12 (fromJust r)) else False
                    | isJust l && isNothing r    = if (getRoot (fromJust l)) < m then prob12 (fromJust l) else False
                    | isNothing l && isJust r    = if (getRoot (fromJust r)) > m then prob12 (fromJust r) else False
                    | isNothing l && isNothing r = True

------------------------------------------------------------
-- PROBLEM #13
--
-- На вход подаётся значение и дерево поиска. Вернуть то
-- поддерево, в корне которого находится значение, если оно
-- есть в дереве поиска; если его нет - вернуть Nothing
getRoot (Tree l m r) = m
getLeft  (Tree l m r) = l
getRight (Tree l m r) = r

prob13 :: Ord a => a -> Tree a -> Maybe (Tree a)
prob13 x tree | getRoot tree == x = Just tree
              | not (getRoot tree == x) && (isNothing (getLeft tree)) && (isNothing (getRight tree)) = Nothing
              | isJust (getRight tree) && isNothing (getLeft tree) = if (not ((prob13 x right) == Nothing)) then (prob13 x right) else Nothing
              | isNothing (getRight tree) && isJust (getLeft tree) = if (not ((prob13 x left) == Nothing)) then (prob13 x left) else Nothing
              | isJust (getRight tree) && isJust (getLeft tree) = if (not ((prob13 x left) == Nothing)) then (prob13 x left) else if (not ((prob13 x right) == Nothing)) then (prob13 x right) else Nothing
                      where
                         left  = fromJust (getLeft  tree)
                         right = fromJust (getRight tree)
------------------------------------------------------------
-- PROBLEM #14
--
-- Заменить () на числа в порядке обхода "правый, левый,
-- корень", начиная с 1
prob14 :: Tree () -> Tree Int
prob14 = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #15
--
-- Выполнить вращение дерева влево относительно корня
-- (https://en.wikipedia.org/wiki/Tree_rotation)
prob15 :: Tree a -> Tree a
prob15 (Tree (Just (Tree  lm )) m (Just (Tree rl rm rr))) = Tree (Just (Tree (Just (Tree Nothing lm Nothing )) m rl)) rm rr

------------------------------------------------------------
-- PROBLEM #16
--
-- Выполнить вращение дерева вправо относительно корня
-- (https://en.wikipedia.org/wiki/Tree_rotation)
prob16 :: Tree a -> Tree a
prob16 (Tree (Just (Tree ll lm lr)) m (Just (Tree  rm ))) = Tree ll lm (Just (Tree lr m (Just (Tree Nothing rm Nothing))))

------------------------------------------------------------
-- PROBLEM #17
--
-- Сбалансировать дерево поиска так, чтобы для любого узла
-- разница высот поддеревьев не превосходила по модулю 1
-- (например, преобразовать в полное бинарное дерево)
prob17 :: Tree a -> Tree a
prob17 = error "Implement me!"
