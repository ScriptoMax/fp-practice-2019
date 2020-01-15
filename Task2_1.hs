module Task2_1 where

{-
  Задание 2.1
  На основе структуры бинарного дерева из лекции необходимо реализовать свою структуру данных
  бинарного дерева поиска (без балансировки) и все операции, приведённые в данном файле
-}

--import Todo(todo)

-- Ассоциативный массив на основе бинарного дерева поиска
-- Ключи - Integer, значения - произвольного типа
data TreeMap v = Leaf | Node (Integer, v) (TreeMap v) (TreeMap v) deriving (Ord, Eq, Show) 

-- Пустое дерево
emptyTree :: TreeMap v 
emptyTree = Leaf 

-- Содержится ли заданный ключ в дереве?
contains :: TreeMap v -> Integer -> Bool
contains Leaf k = False
contains (Node pair tree_r tree_l) k
	| fst pair == k = True 
	| fst pair > k = contains tree_r k
	| otherwise = contains tree_l k 

-- Значение для заданного ключа
lookup' :: Integer -> TreeMap v -> v
lookup' k Leaf = error "No matching key found"
lookup' k (Node pair tree_r tree_l)
	| k == fst pair = snd pair
	| k > fst pair = lookup' k tree_l
	| otherwise = lookup' k tree_r

-- Вставка пары (ключ, значение) в дерево
insert' :: (Integer, v) -> TreeMap v -> TreeMap v
insert' (k, v) Leaf = Node (k, v) Leaf Leaf
insert' (k, v) (Node pair tree_r tree_l)
	| k == fst pair = error "A node with same key already exists"
	| k < fst pair = Node pair (insert' (k, v) tree_r) tree_l 
	| otherwise = Node pair tree_r (insert' (k, v) tree_l)

-- Удаление элемента по ключу
remove :: Integer -> TreeMap v -> TreeMap v
remove i t = todo

-- Поиск ближайшего снизу ключа относительно заданного
nearestLE :: Integer -> TreeMap v -> (Integer, v)
nearestLE i x@(Node pair tree_l tree_r) = if i < fst pair 
														then error "There is no key which is less than input one" else findCloser pair x 
	where
	findCloser :: (Integer, v) -> TreeMap v -> (Integer, v)
	findCloser tempClosest Leaf = tempClosest
	findCloser tempClosest (Node (key, value) tree_l tree_r) = if i < key 
																		then findCloser tempClosest tree_l
																		else
																				if i == key 
																						then(key, value)
																						else findCloser (key, value) tree_r

-- Построение дерева из списка пар
treeFromList :: [(Integer, v)] -> TreeMap v
treeFromList [] = Leaf
treeFromList [x] = insert' x Leaf
treeFromList (x:xs) = insert' x (treeFromList xs) 

-- Построение списка пар из дерева
listFromTree :: TreeMap v -> [(Integer, v)]
listFromTree Leaf = []
listFromTree (Node pair tree_r tree_l) = pair : listFromTree tree_r ++ listFromTree tree_l

-- Поиск k-той порядковой статистики дерева
kMean :: Integer -> TreeMap v -> (Integer, v)
kMean i t = todo
