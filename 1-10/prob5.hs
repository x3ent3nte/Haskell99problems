main = print (rev [1,2,3,4,5])

rev :: [a]  -> [a]
rev list = revx list []

revx :: [a] -> [a] -> [a]
revx [] newlist = newlist
revx (x:xs) newlist = revx xs ([x] ++ newlist)