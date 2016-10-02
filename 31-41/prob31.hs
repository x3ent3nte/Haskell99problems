main = print( isPrime 104729)

isPrime :: Int -> Bool
isPrime num = isPrimex num 2

isPrimex :: Int -> Int -> Bool
isPrimex num factor
	| (factor ^ 2) > num = True
	| (mod num factor) == 0 = False
	| otherwise = isPrimex num (factor+1)