f1 :: Integer -> Integer -> Integer -> Integer -> Bool 
f1 c1 c2 r1 r2
   | c1==c2 && r1==r2 = True
   | c1==r1 && c2==r2 = True
   | c1==r2 && c2==r1 = True
  | otherwise = False 

f2 :: (Integer, Integer, Integer, Integer) -> Bool
f2 (c1, c2, r1, r2)
   |  c1==c2 && r1==r2 = True
   | c1==r1 && c2==r2 = True
   | c1==r2 && c2==r1 = True
   | otherwise = False 