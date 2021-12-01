-- Лабораторна робота №4
-- студента групи КН-32
-- підгрупа 1
-- Комарніцький І.В.
-- Варіант №15

-- Мета: Ознайомитись з системою типiв та класiв типiв. Набути досвiду визначення
-- нових типiв та класiв типiв i їх використання.

-- Тести: 

-- *Main> funk1 [(Mettings "dfg" "dfgg" "dfg" "Ne v")]
-- [Mettings "dfg" "dfgg" "dfg"]


data Notebook
    = Phone String String [String]
   | Alert String String
   | Mettings String String String String
  deriving (Eq, Show)

funk1 :: [Notebook] -> [Notebook]
funk [] = []
funk1 ((Alert a b ): fs) = funk1 fs
funk1 ((Phone a b c): fs) = funk1 fs
funk1 ((Mettings a b c d) : fs) =
    if d == "Ne v"
        then Mettings a b c d : funk1 fs
        else funk1 fs

-- Висновок: Під час виконання лабораторної роботи, ми ознайомилися та 
-- імплементували класи типів мови Haskell. Також ознайомилися з системою
-- типів та класів типів, визначили власні функції для нового типу.