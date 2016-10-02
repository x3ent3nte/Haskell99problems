main = print(myPack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'])

myPack :: [Char] -> [String]
myPack list = myPackX  (tail list) (head list) "" []

myPackX :: [Char] -> Char -> String -> [String] -> [String]
myPackX [] char str newlist = (newlist ++ [(str ++ [char])])
myPackX (x:xs) char str newlist
	| x == char = myPackX xs x (str ++ [x]) newlist
	| x /= char = myPackX xs x "" (newlist ++ [(str ++ [char])])