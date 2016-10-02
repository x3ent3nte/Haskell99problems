main = print(number [1,2,3,4,5,6,7,8,9])

numOfElements :: [a] -> Int
numOfElements list = numOfElementsCalc list 0 

numOfElementsCalc :: [a] -> Int -> Int
numOfElementsCalc [] n = n
numOfElementsCalc list n = numOfElementsCalc (tail list) (n+1)

number :: [a] -> Int
number [] = 0
number (x:xs) = 1 + (number xs) 