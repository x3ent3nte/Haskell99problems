main = print( enumerate 4 12 )

enumerate :: Int -> Int -> [Int]
enumerate start end 
	| start == end = [end]
	| start < end = [start] ++ (enumerate (start+1) end) 
	| start > end = [start] ++ (enumerate (start-1) end) 