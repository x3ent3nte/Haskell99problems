main = print( seclast [1,2,3,4,5,54,11])

seclast:: [a] -> a
seclast [] = error "empty list!"
seclast [x,_,_] = x
seclast (x:xs) = seclast xs