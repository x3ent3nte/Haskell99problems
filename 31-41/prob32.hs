main = print( gcdx 81 63 )

gcdx :: Int -> Int -> Int
gcdx x y 
	| x == y = x
	| x > y = gcdx (x-y) y
	| x < y = gcdx x (y-x)