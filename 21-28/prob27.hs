main = print( length (groupsets ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"] [2,3,4]) )
--main = print( disunion ["a","b","c"] ["a","d"] )

groupsets :: Eq a => [a] -> [Int] -> [[[a]]]
groupsets set [y] = [subsets set y]
groupsets set (y:ys) = nestsets (subsets set y) ys set

nestsets :: Eq a => [[a]] -> [Int] -> [a] -> [[[a]]]
nestsets [] nums ori = []
nestsets (x:xs) [y] ori = (productxx x (subsets (disunion ori x) y)) ++ (nestsets xs [y] ori)  
nestsets (x:xs) (y:ys) ori = (productx x (nestsets (subsets (disunion ori x) y)  ys (disunion ori x)) )  ++ (nestsets xs (y:ys) ori)

subsets :: Eq a => [a] -> Int -> [[a]]
subsets [] num = []
subsets list 1 = decomp list
subsets (x:xs) num = (productx x (subsets xs (num-1))) ++ (subsets xs num)

decomp :: Eq a => [a] -> [[a]]
decomp [] = []
decomp (x:xs) = [[x]] ++ (decomp xs)

productxx :: Eq a => [a] -> [[a]] -> [[[a]]]
productxx list [] = []
productxx list (x:xs) = [[list] ++ [x]] ++ (productxx list xs)

productx :: Eq a => a -> [[a]] -> [[a]]
productx elem [] = []
productx elem (x:xs) = [ [elem] ++ x ] ++ (productx elem xs)

disunion :: Eq a => [a] -> [a] -> [a]
disunion [] set = []
disunion (x:xs) set 
	| inset x set = disunion xs set
	| otherwise = [x] ++ (disunion xs set)

inset :: Eq a => a -> [a] -> Bool
inset elem [] = False
inset elem (x:xs)
	| elem == x = True
	| otherwise = inset elem xs 