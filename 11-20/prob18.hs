main = print( splicey [1,2,3,4,5,6,7,8,9] 3 6 )

splicey :: [a] -> Int -> Int -> [a]
splicey [] start end = error "Empty List"
splicey list 0 end = splicex list end
splicey (x:xs) start end = splicey xs (start-1) (end-1)

splicex :: [a] -> Int -> [a]
splicex [] end = error "Empty List"
splicex (x:xs) 0 = [x]
splicex (x:xs) end = [x] ++ splicex xs (end-1)
