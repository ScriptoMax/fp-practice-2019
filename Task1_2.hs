module Task1_2 where

import Todo(todo)

-- синус числа (формула Тейлора)
sin :: Double -> Double
sin x = todo

-- косинус числа (формула Тейлора)
cos :: Double -> Double
cos x = todo

-- наибольший общий делитель двух чисел
gcd' :: Integer -> Integer -> Integer
gcd' x y
    | x == 1 || y == 1 = 1
	  | x `mod` y == 0 = y
    | otherwise = gcd' y (x - (x `div` y) * y) 


-- существует ли полный целочисленный квадрат в диапазоне [from, to)?
doesSquareBetweenExist :: Integer -> Integer -> Bool
doesSquareBetweenExist from to = todo

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect day month year
    | day < 1 || month < 1 || year < 1 = False
    | day > 31 || month > 12 = False
    | day == 29 && month == 2 && not(isLeap year) = False
	  | day > 29 && month == 2 = False
	  | day > 30 && month `elem` [4, 6, 9, 11] = False
    | otherwise = True	

    where 
    isLeap x
        | x `mod` 4 == 0 && x `mod` 100 /= 0 || x `mod` 400 == 0 = True
        | otherwise = False	

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
pow :: Integer -> Integer -> Integer
pow x 0 = 0
pow x 1 = x
pow x y = x * pow x (y-1) 

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime 1 = True
isPrime x = if x <= 0 then error "No prime number evaluation for 0 and negative arguments" else aux x 2
    where
    aux val x
	| val == x = True
	| val `mod` x == 0 = False
	| otherwise = aux val (x + 1)

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
shapeArea points = todo

-- треугольник задан своими координатами.
-- функция должна вернуть
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Point2D -> Point2D -> Point2D -> Integer
triangleKind a b c = todo
