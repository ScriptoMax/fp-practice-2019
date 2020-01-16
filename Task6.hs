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

number :: Parser Double
number = try calcDouble <|> calcInt  

calcInt :: Parser Double 
calcInt = read <$> many1 digit

calcDouble :: Parser Double 
calcDouble = do
    integ <- many1 digit
    char '.'
    decim <- many1 digit
    pure $ read $ integ <> "." <> decim	 
 
numberFact :: Parser Integer  -- particularly to let parser consume integers strictly as `factorial` inputs     
numberFact = read <$> many1 digit

fp_char :: Parser String 
fp_char = many1 digit

applyMany :: a -> [a -> a] -> a
applyMany x [] = x
applyMany x (h:t) = applyMany (h x) t

div_ :: Parser (Double -> Double -> Double)
div_= do
	char '/'
	return (/)
	
star :: Parser (Double -> Double -> Double)
star = do
	char '*'	
	return (*)	
	
plus :: Parser (Double -> Double -> Double)
plus = do
	char '+'	
	return (+)
	
minus :: Parser (Double -> Double -> Double)
minus = do
	char '-'	
	return (-)	
	
multiplication :: Parser Double
multiplication = do
	spaces
	lhv <- atom <|> fact' <|> negation'
	spaces
	t <- many tail
	return $ applyMany lhv t
	where tail =
				do
					f <- star <|> div_
					spaces
					rhv <- atom <|> fact' <|> negation'
					spaces
					return (`f` rhv)
 	
atom :: Parser Double
atom = number <|> do
	char '('
	res <- addition
	char ')'
	return res

-- factorial  - `!` set in prior to a number because test function calls have been skipped when `!` followed that number. 
-- A bit confusing, so it's a kind of whatever to be working) 	
fact' :: Parser Double
fact' = do
    spaces
    char '!'
    rhv <- numberFact
    return $ factorial rhv

factorial :: Integer -> Double
factorial n
	| n < 0 = error "No factorial exists for negative inputs" 
	| n == 0 || n == 1 = 1
	| otherwise = acc n 1
	where
	acc 0 a = a
	acc b a = acc (b-1) (fromIntegral b * a) 	

-- negation	
negation' :: Parser Double
negation' = do
	spaces
	char '~'
	rhv <- atom
	spaces
	return $ negate rhv	

-- core function to (re)run parser tests calling `parseTest addition {some test string input}`	
addition :: Parser Double
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
					

	
