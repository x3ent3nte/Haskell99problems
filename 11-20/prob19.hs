main = print ( myshift ['a','b','c','d','e','f','g','h'] 4 )

myshift :: [a] -> Int -> [a]
myshift [] num = error "Empty List" 
myshift list num = shiftx list (mod num (length list)) []

shiftx :: [a] -> Int -> [a] -> [a]
shiftx [] num newlist = error "Empty List"
shiftx list 0 newlist = list ++ newlist
shiftx (x:xs) num newlist = shiftx xs (num-1) (newlist++[x])