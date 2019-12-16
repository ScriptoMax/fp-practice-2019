module Task3_1 where

{-
  Задание 3.1
  Числа Пеано: представление чисел на основе нуля и операций "+1" и "-1".
  Необходимо реализовать все классы типов, характерные для целых чисел.
-}

data WeirdPeanoNumber = Zero | Succ WeirdPeanoNumber | Pred WeirdPeanoNumber 

instance Num WeirdPeanoNumber where
	(+) = addition'
	(*) = product'
	(-) = subtraction'
	signum n = 1
	abs = id
	fromInteger = fromInteger'

instance Ord WeirdPeanoNumber where
	(<=) = leq'

instance Eq WeirdPeanoNumber where
	Zero == Zero = True
	x == Zero = False
	Zero == y = False
	(Succ x) == (Succ y) = True
	
instance Show WeirdPeanoNumber where
	show Zero = "Zero"
	show (Succ a) = "Succ " ++ show a
	show (Pred a) = "Pred " ++ show a


leq' :: WeirdPeanoNumber -> WeirdPeanoNumber -> Bool
leq' Zero Zero = True
leq' x Zero = False
leq' Zero y = True
leq' x y = x == y || leq' x y

succ' :: WeirdPeanoNumber -> WeirdPeanoNumber
succ' x = Succ x

pred' :: WeirdPeanoNumber -> WeirdPeanoNumber
pred' Zero = Zero
pred' x = Pred x


toInt' :: WeirdPeanoNumber -> Int   
toInt' Zero = 0
toInt' (Succ x) = temp' x 1 
	where
	temp' Zero n = n
	temp' (Succ x) n = temp' x (n + 1)	

addition' :: WeirdPeanoNumber -> WeirdPeanoNumber -> WeirdPeanoNumber
addition Zero Zero = Zero
addition' x Zero = x
addition' Zero y = y 
addition' x y = addition' (Pred x) (Succ y)

subtraction' :: WeirdPeanoNumber -> WeirdPeanoNumber -> WeirdPeanoNumber
subtraction' Zero Zero = Zero
subtraction' x Zero = x
subtraction' Zero y = Zero 
subtraction' x y = subtraction' (Succ x) (Succ y)

product' :: WeirdPeanoNumber -> WeirdPeanoNumber -> WeirdPeanoNumber
product' Zero Zero = Zero
product' x Zero = Zero
product' Zero y = Zero
product' x (Succ y) = x + (product' x y)

fromInt' :: Int -> Maybe WeirdPeanoNumber
fromInt' 0 = Just Zero
fromInt' n
	| n < 0 = Nothing
	| otherwise = temp_1' n Zero
	where
	temp_1' x y = if (x > 0) then temp_1' (x-1) (Succ y) else Just y

fromInteger' :: Integer -> WeirdPeanoNumber
fromInteger' 0 = Zero
fromInteger' x
	| x < 0 = Zero
	| otherwise = temp_2' x Zero
	where
	temp_2' x y = if (x > 0) then temp_2' (x-1) (Succ y) else y  	