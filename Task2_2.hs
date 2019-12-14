module Task2_2 where

{-
  Задание 2.2
  Необходимо реализовать функции foldl, foldr и unfoldr, а также все остальные функции
  в данном файле _на основе этих трёх_
-}

import Todo(todo)

import Prelude hiding (foldl, foldr, unfoldr, map, concatMap,
    filter, maxBy, minBy, reverse, sum, product, elem)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f g [] = g
foldl f g [x] = f g x
foldl f g (x:xs) = foldl f (f g x) xs 

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f g [] = g
foldr f g [x] = f x g
foldr f g (x:xs) = f x (foldr f g xs) 

unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr x y = 
	case x y of 
			Nothing -> []
			Just (a, b) -> a : unfoldr x y

-- Сумма всех элементов списка (пример)
sum :: [Integer] -> Integer
sum lst = foldl (+) 0 lst

-- Переворот списка (Пример)
reverse :: [a] -> [a]
reverse lst = foldl f [] lst where f t h = h:t

-- Отображение элементов списка
map :: (a -> b) -> [a] -> [b]  
map f [] = []
map f (x:xs) = f x : map f xs 

-- Произведение всех элементов списка
product :: [Integer] -> Integer
product [] = 0
product [x] = x
product lst = foldl (*) 1 lst

-- Выделение из списка Maybe всех существующих значений
catMaybes :: [Maybe a] -> [a]
catMaybes = foldr (maybe id (:)) []

-- Диагональ матрицы
diagonal :: [[a]] -> [a]
diagonal = todo

-- Фильтр для всех элементов, не соответствующих предикату
filterNot :: (a -> Bool) -> [a] -> [a]
filterNot f [] = []
filterNot f (x:xs) = foldr (\x xs -> if f x then x:xs else xs) [] xs

-- Поиск элемента в списке
elem :: (Eq a) => a -> [a] -> Bool
elem n [] = False
elem n [x] = n == x
elem n (x:xs) = foldr (\x -> (||) (n == x)) False xs 

-- Список чисел в диапазоне [from, to) с шагом step
rangeTo :: Integer -> Integer -> Integer -> [Integer]
rangeTo from to step = unfoldr (\from -> if from >= to then Nothing else Just (from, from + step)) from 

-- Конкатенация двух списков
append :: [a] -> [a] -> [a]
append [] [] = []
append [] xs = xs
append xs [] = xs
append xs ys = foldl (flip(:)) ys (reverse xs)

-- Разбиение списка lst на куски размером n
-- (последний кусок может быть меньше)
groups :: [a] -> Integer -> [[a]]
groups lst n = todo
