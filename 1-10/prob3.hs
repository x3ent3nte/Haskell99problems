main= print (kth [63,17,46,13,7,953,52] 6)

kth :: [a] -> Int -> a
kth [] n = error "empty list!"
kth nums 1 = head nums
kth (x:xs) n = kth xs (n-1)