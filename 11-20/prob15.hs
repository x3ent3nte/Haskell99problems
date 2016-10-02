main = print ( nnduplicate ['a','b','c','d','e'] 3 1 )

nnduplicate :: [a] -> Int -> Int -> [a]
nnduplicate list num 1 = nduplicate list num
nnduplicate list num n = nduplicate (nnduplicate list num (n-1)) num

nduplicate :: [a] -> Int -> [a]
nduplicate [] num = []
nduplicate (x:xs) num = (nreplicate x num) ++ (nduplicate xs num) 

nreplicate :: a -> Int -> [a]
nreplicate elem 1 = [elem]
nreplicate elem num = [elem] ++ (nreplicate elem (num-1))