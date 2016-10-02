main = print ( insertx "abcdefghijklmnopqrstuvwxyz" "XXX" 10 )

insertx :: [a] -> [a] -> Int -> [a]
insertx [] str num = error "Empty List!"
insertx list str 0 = str ++ list
insertx (x:xs) str num = [x] ++ insertx xs str (num-1) 