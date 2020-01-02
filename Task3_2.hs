module Task3_2 where

{-
  Задание 3.2
  Структура данных "перевёрнутый список" представляет собой зеркально обращённый
  односвязный список из стандартной библиотеки, для которого взятие последнего
  элемента является наиболее эффективной операцией.
  Необходимо реализовать все функции и классы типов в данном файле.
-}

import Todo(todo)

data ReverseList a = RNil | RCons (ReverseList a) a 

rlistToList :: ReverseList a -> [a]
rlistToList RNil = []
rlistToList x@(RCons r a) = lst_acc [] x 
	where 
	lst_acc n RNil = n
	lst_acc n (RCons temp_r temp_a) = temp_a : lst_acc n temp_r
	
listToRList :: [a] -> ReverseList a
listToRList [] = RNil
listToRList [x] = RCons RNil x
listToRList (x:xs) = RCons (listToRList xs) x

-- Реализуйте классы Eq, Ord, Show, Monoid, Functor

instance (Eq a) => Eq (ReverseList a) where 
	(==) RNil RNil = True
	(==) RNil x = False 
	(==) x RNil = False
	(==) (RCons RNil x) (RCons RNil y) = if x == y then True else False
	(==) (RCons rev1 x) (RCons rev2 y) = if x == y && rev1 == rev2 then True else False
	
-- As suggested, an Ord instance is necessary to arrange various 'ReverseList's strictly by length. 
--That's a personal grasp, just not sure I caught on sense as intended    	
instance (Ord a) => Ord (ReverseList a) where
	(<=) RNil RNil = True
	(<=) RNil x = True
	(<=) x RNil = False 
	(<=) (RCons RNil x) (RCons RNil y) = True
 	(<=) (RCons rev1 x) (RCons rev2 y) = if length (rlistToList rev1) <= length (rlistToList rev2) then True else False 

instance (Show a) => Show (ReverseList a) where
	show RNil = "RNil"
	show (RCons RNil a) = "RCons RNil " ++ show a
	show (RCons x a) = "RCons " ++ "(" ++ show x ++ ") " ++ show a 

instance Monoid (ReverseList a) where
	mempty = RNil
	mappend mempty x = x
	mappend x mempty = x	
	mappend x (RCons mempty y) = RCons x y
	mappend x (RCons t y) = RCons (mappend x t) y 	

-- Semigroup instance to resume defining Monoid instance, GHC found Monoid stuff as erroneous one until 
-- fixing that with a Semigroup instance (particularly for ReverseList)           	
instance Semigroup (ReverseList a) where
	RNil <> RNil = RNil 
	RNil <> RCons x y = RCons x y
	RCons x y <> RNil = RCons x y
	RCons x y <> RCons t u = RCons x y
	
instance Functor ReverseList where
	fmap f RNil = RNil
	fmap f (RCons x a) = RCons (f `fmap` x) (f a)	