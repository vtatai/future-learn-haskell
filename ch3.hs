
length' [] = 0
length' (x:xs) = 1 + length' xs

filter' pred [] = []
filter' pred (x:xs) 
  | pred x = x : (filter' pred xs)
  | otherwise = filter' pred xs
