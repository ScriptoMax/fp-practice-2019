module Task3_3 where

{-
  Задание 3.3
  Множество на основе предикатов
-}

newtype PSet a = PSet{ contains :: (a -> Bool) }

-- Реализуйте классы Monoid и Functor
-- Объясните в комментариях, почему они реализованы именно так
