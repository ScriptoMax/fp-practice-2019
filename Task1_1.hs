module Task1_1 where

import Todo(todo)

data Term = IntConstant{ intValue :: Int }           -- числовая константа
            | Variable{ varName :: String }          -- переменная
            | BinaryTerm{ lhv :: Term, rhv :: Term } -- бинарная операция
            deriving(Show,Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
(|+|) :: Term -> Term -> Term
(|+|) (IntConstant l)(IntConstant r) = IntConstant (l + r)
(|+|) (BinaryTerm (IntConstant l1)(IntConstant r1))(BinaryTerm (IntConstant l2)(IntConstant r2)) = BinaryTerm (IntConstant (l1 + r1))(IntConstant (l2 + r2))
(|-|) :: Term -> Term -> Term
(|-|) (IntConstant l)(IntConstant r) = IntConstant (l - r)
(|-|) (BinaryTerm (IntConstant l1)(IntConstant r1))(BinaryTerm (IntConstant l2)(IntConstant r2)) = BinaryTerm (IntConstant (l1 - r1))(IntConstant (l2 - r2))
(|*|) :: Term -> Term -> Term
(|*|) (IntConstant l)(IntConstant r) = IntConstant (l * r)
(|*|) (BinaryTerm (IntConstant l1)(IntConstant r1))(BinaryTerm (IntConstant l2)(IntConstant r2)) = BinaryTerm (IntConstant (l1 * r1))(IntConstant (l2 * r2))

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
evaluate expression = todo
