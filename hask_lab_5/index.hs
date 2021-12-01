-- Лабораторна робота №5
-- студентки групи КН-32 підгрупи 1
-- Комарніцький І.В.
-- Варіант №15

-- 1.8 Повторити n-кратно кожен елемент списку, напр. при n=2: "asd"⇒
-- [’a’, ’a’, ’s’, ’s’, ’d’, ’d’].
import Data.Char

funk1 :: Int -> String -> String
funk1 1 str  = str
funk1 amount str =  concat $ replicate amount str

vyvod :: IO()
vyvod = do
-- a) введення з клавіатури
    putStrLn "Input amount:"
    amount<-getLine
    putStrLn "Input str:"
    str<-getLine
    putStrLn(funk1 (digitToInt (head amount)) str)
--б) введення даних з файлу
    strL<-readFile "input.txt"
--в) виведення результатів на екран
    putStrLn "Input amount:"
    amount<-getLine
    putStrLn "Input str:"
    str<-getLine
    putStrLn(funk1 (digitToInt (head amount)) str)
--г) виведення результатів у файл
    strL<-readFile "input.txt"
    writeFile "output.txt" (funk1 (digitToInt (head amount)) str)

-- Висновок: Ознайомились з модульною органiзацiєю програм та засобами введення-виведення.