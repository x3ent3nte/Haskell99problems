main = print( removeDuplicate ["a","a","a","b","b","a","c","c","c","c","d","d","b","b","b","b","e","e"]) 

removeDuplicate :: [String] -> [String]
removeDuplicate list = removex list (head list) []

removex :: [String] -> String -> [String] -> [String]
removex [] head new = new ++ [head]
removex (x:xs) head new
	| x == head = removex xs head new
	| x /= head = removex xs x (new ++ [head])
