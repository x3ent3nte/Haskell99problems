main = print(mylastproxy mylast [1,2,3,4,5,99])

mylastproxy funcy list = funcy list 

mylast::[a] -> a
mylast []= error "Error: empty list!"
mylast [x]=x
mylast (_:xs) = mylast xs

mylast2::[a] -> a
mylast2 [] = error "empty list!"
mylast2 nums = head (reverse nums)