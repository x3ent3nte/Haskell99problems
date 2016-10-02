data Tuppy a = Single a | Multiple Int a  deriving (Show)

main = print( myDecode [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e'] )

myDecode :: [Tuppy a] -> [a]
myDecode list = decodex list

decodex :: [Tuppy a] -> [a]
decodex [] = []
decodex (x:xs) = (attach x) ++ (decodex xs) 

attach :: Tuppy a -> [a]
attach (Single a) = [a]
attach (Multiple num a) = attachx num a

attachx :: Int -> a -> [a]
attachx 1 a = [a]
attachx num a = [a] ++ attachx (num-1) a