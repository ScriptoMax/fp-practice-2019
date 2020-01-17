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

-- try obtaining some floating point (primarily) or integer numbers from string input 
number :: Parser Double
number = try calcDouble <|> calcInt  

-- integer handler 
calcInt :: Parser Double 
calcInt = read <$> many1 digit

-- floating point handler 
calcDouble :: Parser Double 
calcDouble = do
    integ <- many1 digit
    char '.'
    decim <- many1 digit
    pure $ read $ integ <> "." <> decim	 
 
-- defined particularly to let parser consume integers as a `factorial` normal sort of arguments 
numberFact :: Parser Integer         
numberFact = read <$> many1 digit

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
	lhv <- try fact' <|> atom <|> negation'
	spaces
	t <- many tail
	return $ applyMany lhv t
	where tail =
				do
					f <- star <|> div_
					spaces
					rhv <- try fact' <|> atom <|> negation'
					spaces
					return (`f` rhv)
 	
atom :: Parser Double
atom = number <|> do
	char '('
	res <- addition
	char ')'
	return res

-- factorial argument picker  
fact' :: Parser Double
fact' = do
    spaces
    rhv <- numberFact
    char '!'
    return $ factorial rhv

-- factorial computation comes in action once picker succeeds fetching valid argument  
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

-- core function to (re)run parser tests (command to call parser: `parseTest addition "{some test string input}"`)	
addition :: Parser Double
addition = do
	spaces
	lhv <- multiplication <|> negation'
	spaces
	t <- many tail
	return $ applyMany lhv t
	where tail =
				do
					f <- plus <|> minus
					spaces
					rhv <- try multiplication <|> negation'
					spaces
					return (`f` rhv)