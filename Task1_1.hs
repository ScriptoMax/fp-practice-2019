module Task1_1 where

import Todo(todo)

data Term = IntConstant{ intValue :: Int }           -- числовая константа
            | Variable{ varName :: String }          -- переменная
            | BinaryTerm{ lhv :: Term, rhv :: Term } -- бинарная операция
            deriving(Show,Eq)

data Math_operators = Addition | Subtraction | Product deriving(Show,Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
(|+|) :: Term -> Term -> Term
(|+|) :: Term -> Term -> Term
(|+|) a b = BinaryTerm a Addition b   			
			
(|-|) :: Term -> Term -> Term
(|-|) a b = BinaryTerm a Subtraction b 

(|*|) :: Term -> Term -> Term
(|*|) a b = BinaryTerm a Product b 				
			
infixl 0 |+|
infixl 0 |-|
infixl 1 |*|  

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement expression = 
    case expression of
        Variable exp -> if (exp == varName) then replacement else expression
        BinaryTerm exp1 exp2 -> BinaryTerm (replaceVar varName replacement exp1)(replaceVar varName replacement exp2)	

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate expression = 
	case expression of 
		BinaryTerm a op b ->
			case(evaluate (a), op, evaluate (b)) of
				(IntConstant a, Addition, IntConstant b) -> IntConstant(a + b)
				(IntConstant a, Subtraction, IntConstant b) -> IntConstant(a - b)
				(IntConstant a, Product, IntConstant b) -> IntConstant(a * b)
				_ -> BinaryTerm a op b
		_ -> expression		
