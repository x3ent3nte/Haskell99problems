main = print( removekth "abcdefghijklmnopqrstuvwxyz" 9)

removekth :: [a]-> Int -> [[a]]
removekth list num = getend (removey list num)

getend :: [a] -> [[a]]
getend list = [[last list]] ++ [init list] 

removey :: [a] -> Int -> [a]
removey [] num = error "Empty List"
removey (x:xs) 0 = xs ++ [x]
removey (x:xs) num = [x] ++ (removey xs (num-1))