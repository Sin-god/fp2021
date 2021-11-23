--Лабораторна робота №2
--студентa групи КН-32 підгрупа 1
--Комарніцького Іллі
--Варіант 15

--Мета:Набути досвiду визначення та використання функцiй вищого порядку.

--Визначте вказанi функцiї в кожному з завдань: а) без застосування, б) з
--застосуванням вбудованих функцiй.

import Data.List ()
import System.IO ()


-- Завдання 1. Роздiлити список на двi частини при заданiй довжинi першої n,
-- напр. при n=3: "abcdefghik"⇒ ("abc "defghik")

--а) без застосування вбудованих функцій
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

divideListN' :: [a] -> Int -> ([a], [a])
divideListN' [] _ = ([],[])
divideListN' all@(x:xs) n
    | n > 0 = (x:ys, zs)
    | otherwise = ([], all)
    where 
        (ys, zs) = divideListN' xs (n-1)

 --Тестування

-- *Main> divideListN' "HelloMrsMom" 3  
-- ("Hel","loMrsMom")
-- *Main> divideListN' "HelloMrsMom" 20
-- ("HelloMrsMom","")
-- *Main> divideListN' "HelloMrsMom" 0 
-- ("","HelloMrsMom")
-- *Main> divideListN' "HelloMrsMom" 0 
-- ("","HelloMrsMom")

--б) з застосуванням вбудованих функцiй

divideListN :: [a] -> Int -> ([a], [a])
divideListN [] _ = error "The list is empty"
divideListN xs 0 = error "Can't divide a list to (0, _)"
divideListN xs n
    | n < l = splitAt n xs
    | otherwise = error "n is more or equal than lenght"
    where 
        l = length xs


 --Тестування

-- *Main> divideListN "1234567" 5
-- ("12345","67")

-- *Main> divideListN "123456" 7
-- *** Exception: n is more or equal than lenght
-- CallStack (from HasCallStack):
--   error, called at index.hs:49:19 in main:Main

-- *Main> divideListN "" 8
-- *** Exception: The list is empty
-- CallStack (from HasCallStack):
--   error, called at index.hs:45:20 in main:Main

-- *Main>  divideListN "152112112444545446445" 0
-- *** Exception: Can't divide a list to (0, _)
-- CallStack (from HasCallStack):
--   error, called at index.hs:46:20 in main:Main

-- *Main> divideListN "HelloMrsMom" 4                  
-- ("Hell","oMrsMom")


--Завдання 2. Перевiрити гiпотезу Ґольдбаха у вказаному дiапазонi.

--а) без застосування вбудованих функцій

myHead :: [a] -> a
myHead (x:xs) = x

goldbach :: Int -> (Int, Int)
goldbach a = myHead $
                    filter (\(x,y) -> isPrime x && isPrime y) $
                    map (\c -> (c, a - c)) [3,5..a `div` 2]
  where
  factors a = filter (isFactor a) [2..a-1]
  isFactor a b = a `mod` b == 0
  isPrime 1 = False
  isPrime a = null $ factors a

goldbachRange :: [Int] -> [(Int, Int)]
goldbachRange [] = []
goldbachRange (x:xs) = goldbach x : goldbachRange xs

--Тестування

-- *Main> goldbachRange [1..20]
-- [*** Exception: index.hs:83:1-17: Non-exhaustive patterns in function myHead

-- *Main> goldbachRange [1,4..10]
-- [*** Exception: index.hs:83:1-17: Non-exhaustive patterns in function myHead

-- *Main> goldbachRange [6,10..17]
-- [(3,3),(3,7),(3,11)]

-- *Main> goldbachRange [14,16..25]
-- [(3,11),(3,13),(5,13),(3,17),(3,19),(5,19)]


--б) з застосуванням вбудованих функцiй

goldbach' :: Int -> (Int, Int)
goldbach' a = head $
                    filter (\(x,y) -> isPrime x && isPrime y) $
                    map (\c -> (c, a - c)) [3,5..a `div` 2]
  where
  factors a = filter (isFactor a) [2..a-1]
  isFactor a b = a `mod` b == 0
  isPrime 1 = False
  isPrime a = null $ factors a

goldbachRange' :: [Int] -> [(Int, Int)]
goldbachRange' = map goldbach'

 --Тестування

-- *Main> goldbachRange' [1..20]
-- [*** Exception: Prelude.head: empty list
-- *Main> goldbachRange' [4,6..10]
-- [*** Exception: Prelude.head: empty list
-- *Main> goldbachRange' [6,10..17]
-- [(3,3),(3,7),(3,11)]
-- *Main> goldbachRange' [14,16..25]
-- [(3,11),(3,13),(5,13),(3,17),(3,19),(5,19)]

 --Висновок: Під час лабораторної роботи я мав змогу познайомитися та 
 --імплементувати нові функції мови Haskell, а саме: filter, map, null, isPrime та splitAt, 
 --а також дізнався як використовуються та навіщо потрібні такі символи як '.' та '$'.