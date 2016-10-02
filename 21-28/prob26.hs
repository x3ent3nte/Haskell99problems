main = print( (xgroups "abcdefghijklmnopqrstuvwxyz" 3) )

xgroups :: [a] -> Int -> [[a]]
xgroups [] num = []
xgroups list num 
	| (length list) < num = []
xgroups list 1 = decomp list
xgroups (x:xs) num = (productx x (xgroups xs (num-1))) ++ (xgroups xs num)

decomp :: [a] -> [[a]]
decomp [] = []
decomp (x:xs) = [[x]] ++ (decomp xs)

productx :: a -> [[a]] -> [[a]]
productx elem [] = []
productx elem (x:xs) = [[elem] ++ x] ++ (productx elem xs)