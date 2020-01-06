module Task4_2 where

import Data.Functor
import Control.Applicative
import Control.Monad

{-
  Задание 4.1
  Реализация монады над множеством из четырёх элементов.
  Реализуйте все требуемые классы типов и объясните, почему они реализованы именно так.
-}

data FourOf a = FourOf a a a a deriving(Show,Eq)

-- реализуйте классы `Functor`, `Applicative` и `Monad` для типа `FourOf`
-- таким образом, что
-- do { x <- FourOf 1 2 3 4; y <- FourOf 4 6 7 8; return $ x + y } === FourOf 5 8 10 12

{- Экземпляр Functor -
Вызовом `fmap` функция `f` поднимается в контекст функтора, после чего применяется к каждому из аргументов конструктора типа.
-}
instance Functor FourOf where
    fmap f (FourOf a b c d) = FourOf (f a) (f b) (f c) (f d)

{- Экземпляр Applicative -
Результат вызова `pure` - функторный контейнер, все аргументы в конструкторе которого равны аргументу `pure`. 
При реализации <*> заданная функция применяется к соответствующим двойкам аргументов. На выходе образуется функтор, аргументы которого - результаты
локальных приложений функции.       
-}	
instance Applicative FourOf where
    pure x = FourOf x x x x
    (FourOf a b c d) <*> (FourOf p s q t) = FourOf (a p) (b s) (c q) (d t)

{- Экземпляр Monad -
`return` возвращает монадический контейнер с аргументами, идентичными по значению аргументу функции.
Для последовательного применения функции в определении `>>=` используется вспомогательный pattern matching, с помощью которого аргументы конструктора по порядку 
упаковываются в монаду. После поэлементной распаковки получаем конструктор `FourOf` со значениями, в которых учитывается результат отображения `f`.     
-}
instance Monad FourOf where
    return x = FourOf x x x x
    FourOf a b c d >>= f = FourOf a' b' c' d' where
        FourOf a' _ _ _ = f a 
        FourOf _ b' _ _ = f b
        FourOf _ _ c' _ = f c
        FourOf _ _ _ d' = f d	
