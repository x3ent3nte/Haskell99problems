main = print( myEncode "aaaabccaadeeee" )

myEncode :: [Char] -> [(Char,Int)]
myEncode (x:xs) = myEncodeX xs x 1 []

myEncodeX :: [Char] -> Char -> Int -> [(Char,Int)] -> [(Char,Int)]
myEncodeX [] char num newlist = newlist ++ [(char,num)]
myEncodeX (x:xs) char num newlist
	| x == char = myEncodeX xs char (num+1) newlist
	| x /= char = myEncodeX xs x 1 (newlist ++ [(char,num)])