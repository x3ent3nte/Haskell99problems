main = print( sortsize ["abc","de","fgh","de","ijkl","mn","o"] )

sortsize :: [[a]] -> [[a]]
sortsize [] = []
sortsize [x] = [x]
sortsize (x:xs) = ( sortsize (less xs (length x ) ) ) ++ [x] ++  ( sortsize (greater xs (length x ) ) )

less :: [[a]] -> Int -> [[a]]
less [] num = []
less (x:xs) num
	| (length x) < num = [x] ++ (less xs num)
	| otherwise = less xs num

greater :: [[a]] -> Int -> [[a]]
greater [] num = []
greater (x:xs) num
	| (length x) >= num = [x] ++ (greater xs num)
	| otherwise = greater xs num