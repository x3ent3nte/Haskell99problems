main = print( duplicate [1,2,3,4,5,6] )

duplicate :: [a] -> [a]
duplicate [] = []
duplicate (x:xs) = [x] ++ [x] ++ (duplicate xs) 