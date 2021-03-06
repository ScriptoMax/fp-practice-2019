module Task3_3 where

import Data.Monoid
import Data.Functor

{-
  Задание 3.3
  Множество на основе предикатов
-}

newtype PSet a = PSet { contains :: (a -> Bool) }

-- Реализуйте классы Monoid и Functor
-- Объясните в комментариях, почему они реализованы именно так

{- Экземпляр Monoid:
PSet (\x -> False) - пустое множество.    
В качестве identity взято пустое множество. 
Выступая нейтральным вариантом реализации PSet, такое множество не оказывает влияния на результат при вызове mappend, т.е. возвращается 
другой аргумент, переданный в mappend.  
Дано определение mappend для операции union на некотором наборе исходных множеств. В результате вызова функции формируется новое множество, включающее
все предикаты, принадлежащие множествам-аргументам.      
-}   
instance Monoid (PSet a) where
    mempty = PSet (\x -> False)
    mappend mempty (PSet x) = PSet x
    mappend (PSet x) (PSet y) = PSet (\t -> (x t) || (y t)) 

{- Экземпляр Semigroup для реализации Monoid: 
В результате вызова оператора '<>' над парой непустых множеств запускается рекурсия с перебором предикатов, принадлежающих этим множествам.   
-}  		
instance Semigroup (PSet a) where
    PSet x <> PSet y = PSet (\u -> x u) <> PSet (\v -> y v)  

{- Экземпляр Functor не объявлен. Определение fmap

fmap :: Functor f => (a -> b) -> f a -> f b

интерпретируется мною так: применить некоторую функцию к отдельным значениям типа a для возврата содержащего их контейнера с типом b. Предполагаю, что в качестве
функции допускается использование not. В таком случае необходимо вернуть контейнер (PSet a) с противоположными значениями предикатов. Но изменение 
булевых значений в каждом выводе (a -> Bool) вступает в противоречие со значением аргумента a, детерминировавшим b. 

Например, объявим 

let x = PSet (>=10) 

Тогда вызов `contains x 11` вернёт True. Потенциальное обращение этого результата в False означает логическое противоречие, так как условие истинности
остаётся неизменным. С учётом этих рассуждений полагаю, что для рассматриваемого типа нет предпосылок, позволяющих успешно реализовать Functor.                        

-}
