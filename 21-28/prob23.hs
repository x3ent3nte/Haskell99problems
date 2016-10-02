import System.Random

main = print( pickn "abcdefghijklmnopqrstuvwxyz" 10 )

pickn :: [a] -> Int -> [a]
pickn list num = picknx list ((length list) - num)

picknx :: [a] -> Int -> [a]
picknx list 0 = list
picknx list num = do 
		pos <- (randomRIO (0, (length list)))
		picknx (removen list pos) (num-1) 

removen :: [a] -> Int -> [a]
removen [] num = error "empty list"
removen (x:xs) 0 = xs
removen (x:xs) num = [x] ++ (removen xs (num-1))

