main = print( splitty "abcdefghik" 3 )

splitty :: [a] -> Int -> [[a]]
splitty list num = splittyx list num []

splittyx :: [a] -> Int -> [a] -> [[a]]
splittyx [] num new = error "empty list!"
splittyx list 0 new = [new] ++ [list]
splittyx (x:xs) num new = splittyx xs (num-1) (new ++ [x]) 