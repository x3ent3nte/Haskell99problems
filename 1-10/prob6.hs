main = print (palindrome "hanna")

palindrome :: String -> Bool
palindrome word = eqString word (rev word)

rev :: [a]  -> [a]
rev list = revx list []

revx :: [a] -> [a] -> [a]
revx [] newlist = newlist
revx (x:xs) newlist = revx xs ([x] ++ newlist)

eqString :: String -> String -> Bool
eqString [] [] = True
eqString (a:ax) (b:bx)
	| a == b = eqString ax bx
	| a /= b =False