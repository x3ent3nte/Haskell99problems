data Tuppy a = Single a | Multiple a Int  deriving (Show)

main = print( myEncode "aaaabccaadeeee")

myEncode :: Eq a => [a] -> [Tuppy a]
myEncode (x:xs) = myEncodeX xs x 1 []

myEncodeX :: Eq a => [a] -> a -> Int -> [Tuppy a] -> [Tuppy a]
myEncodeX [] char 1 newlist = (newlist ++ [Single char])
myEncodeX [] char num newlist = (newlist ++ [Multiple char num])
myEncodeX (x:xs) char num newlist
	| x == char = myEncodeX xs x (num+1) newlist
	| x /= char && num ==1 = myEncodeX xs x 1 (newlist ++ [Single char])
	| x /= char = myEncodeX xs x 1 (newlist ++ [Multiple char num])
