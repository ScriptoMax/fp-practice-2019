module Task6 where

{-
В этом файле приведён код, написанный (совместными усилиями) на лекции

Модифицируйте представленный тут парсер таким образом, чтобы он поддерживал унарные операции 
(отрицание и факториал), а также числа с плавающей точкой
-}

import Text.Parsec hiding(digit)
import Data.Functor

type Parser a = Parsec String () a

digit :: Parser Char
digit = oneOf ['0'..'9']

number :: Parser Integer
number = read <$> many1 digit

fp_char :: Parser String 
fp_char = many1 digit

fp_number :: Parser Double
fp_number = read <$> parser where
	parser = (++) <$> fp_char <*> (option "" $ (:) <$> char '.' <*> fp_char)

applyMany :: a -> [a -> a] -> a
applyMany x [] = x
applyMany x (h:t) = applyMany (h x) t


-- | integer functions |

div_ :: Parser (Integer -> Integer -> Integer)
div_= do
	char '/'
	return div
	
star :: Parser (Integer -> Integer -> Integer)
star = do
	char '*'	
	return (*)	
	
plus :: Parser (Integer -> Integer -> Integer)
plus = do
	char '+'	
	return (+)
	
minus :: Parser (Integer -> Integer -> Integer)
minus = do
	char '-'	
	return (-)	
	
multiplication :: Parser Integer
multiplication = do
	spaces
	lhv <- atom <|> negation'
	spaces
	t <- many tail
	return $ applyMany lhv t
	where tail =
				do
					f <- star <|> div_
					spaces
					rhv <- atom <|> negation'
					spaces
					return (`f` rhv)
 	
	
addition :: Parser Integer
addition = do
	spaces
	lhv <- multiplication <|> fact' <|> negation'
	spaces
	t <- many tail
	return $ applyMany lhv t
	where tail =
				do
					f <- plus <|> minus
					spaces
					rhv <- multiplication <|> fact' <|> negation'
					spaces
					return (`f` rhv)
					
atom :: Parser Integer
atom = number <|> do
	char '('
	res <- addition
	char ')'
	return res

-- factorial 	
fact' :: Parser Integer
fact' = do
	spaces
	char '!'
	rhv <- atom 
	return $ factorial rhv  
	
factorial :: Integer -> Integer
factorial n
	| n < 0 = error "No factorial exists for negative inputs" 
	| n == 0 || n == 1 = 1
	| otherwise = acc n 1
	where
	acc 0 a = a
	acc b a = acc (b-1) (b * a)  

-- negation	
negation' :: Parser Integer
negation' = do
	spaces
	char '~'
	rhv <- atom
	spaces
	return $ negate rhv	
	
	
-- | fp functions |

fp_div_ :: Parser (Double -> Double -> Double)
fp_div_= do
	char '/'
	return (/)
	
fp_star :: Parser (Double -> Double -> Double)
fp_star = do
	char '*'	
	return (*)	
	
fp_plus :: Parser (Double -> Double -> Double)
fp_plus = do
	char '+'	
	return (+)
	
fp_minus :: Parser (Double -> Double -> Double)
fp_minus = do
	char '-'	
	return (-)

fp_multiplication :: Parser Double
fp_multiplication = do
	spaces
	lhv <- fp_atom <|> fp_negation'
	spaces
	t <- many tail
	return $ applyMany lhv t
	where tail =
				do
					f <- fp_star <|> fp_div_
					spaces
					rhv <- fp_atom <|> fp_negation'
					spaces
					return (`f` rhv) 	
	
fp_addition :: Parser Double
fp_addition = do
	spaces
	lhv <- fp_multiplication <|> fp_negation'
	spaces
	t <- many tail
	return $ applyMany lhv t
	where tail =
				do
					f <- fp_plus <|> fp_minus
					spaces
					rhv <- fp_multiplication <|> fp_negation'
					spaces
					return (`f` rhv)
					
fp_atom :: Parser Double
fp_atom = fp_number <|> do
	char '('
	res <- fp_addition
	char ')'
	return res

fp_negation' :: Parser Double 
fp_negation' = do
	spaces
	char '~'
	rhv <- fp_atom
	spaces
	return $ negate rhv	