--Лабораторна робота №3
--студентки групи КН-32
--Комарніцького Іллі
-- Варыант 15

--Мета:
--Набути досвiду визначення рекурсивних функцiй, використання механiзму
--зiставлення зi зразком i роботи з кортежами та списками.

--Завдання
--Для кожного з завдань описати функції з застосуванням вбудованих функцій
--та без їх застосування

--1.1 Знайти k-й справа елемент списку.
getSecondLastElement :: [a] -> Int  -> a
getSecondLastElement [] k = error "list is empty"
getSecondLastElement [x] k = error "list contains only 1 element"
getSecondLastElement (x:xs) k = helper (x:xs) k 0
helper (x:xs) k amount
    | k == amount = x 
    | otherwise = helper xs k (amount+1)
    


--Тестування
-- getSecondLastElement[1 .. 10] 5
-- 6
--getSecondLastElement[1]
--list contains only 1 element
--getSecondLastElement[]
--list is empty



--2.1 Циклiчний вправо зсув списку на n позицiй з з використанням вбудованих функцій.
cyclicShiftR :: Int -> [a] -> [a]
cyclicShiftR n xs = let (a, b) = splitAt (length xs - n) xs
                    in b ++ a

--Тестування
-- cyclicShiftR 4 [1 .. 10]
-- [7,8,9,10,1,2,3,4,5,6]
--cyclicShiftR(4,[])
--[]
--cyclicShiftR(4,[1 .. 3])
--[1,2,3]

--2.2 Циклiчний правий зсув списку на n позицiйз без використання вбудованих функцій.
shiftListRightRecursion :: (Int, [a]) -> [a]
shiftListRightRecursion  (n, []) = []
shiftListRightRecursion  (0, x) = x
shiftListRightRecursion  (n, x : xs) = shiftListRightRecursion  (n - 1, xs ++ [x])

--Тестування
--shiftListRightRecursion (4 [1 .. 10]
-- [7,8,9,10,1,2,3,4,5,6]
--shiftListRightRecursion (4,[])
--[]
--shiftListRightRecursion (4,[1 .. 3])
--[1,2,3]

--Висновок
--Під час лабораторної роботи ми ознайомились з реалізацією рекурсивних функцій та використанням вбудованих засобів мови  Haskell.
--Також у нас була можливість попрацювати зі списками та кортежами в якості аргументів функцій.
--Порівнюючи рекурсію із вбудованими функціями, варто зазначити, що результат в обох випадках виявився однаковим,
--проте використання вбудованих функцій є зручнішим та більш читабельним.