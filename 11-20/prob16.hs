main = print( dropn "abcdefghijk" 4 )

dropn :: [a] -> Int -> [a]
dropn list num = dropnx list num 1

dropnx :: [a] -> Int -> Int -> [a]
dropnx [] num count = []
dropnx (x:xs) num count
	| count == num = dropnx xs num 1
	| count /= num = [x] ++ dropnx xs num (count+1)