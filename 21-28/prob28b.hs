main = print( sortfreq ["abc", "de", "fgh", "de", "ijkl", "mn", "o"] )

sortfreq :: [[a]] -> [[[a]]]
sortfreq list = sortsize (groupfreq list)

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





groupfreq :: [[a]] -> [[[a]]]
groupfreq [] = [] 
groupfreq (x:xs) = [[x] ++ samelength xs (length x)] ++ (groupfreq (difflength xs (length x) ))

samelength :: [[a]] -> Int -> [[a]]
samelength [] size = []
samelength (x:xs) size
	| (length x) == size = [x] ++ (samelength xs size) 
	| otherwise = samelength xs size

difflength :: [[a]] -> Int -> [[a]]
difflength [] size = []
difflength (x:xs) size
	| (length x) /= size = [x] ++ (difflength xs size) 
	| otherwise = difflength xs size