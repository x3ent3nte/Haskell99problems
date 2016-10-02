data NestedList a = Elem a | List [NestedList a]

main = print(myFlatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]))

myFlatten :: NestedList a -> [a]
myFlatten list = myFlattenX list []

myFlattenX :: NestedList a -> [a] -> [a]
myFlattenX  (List []) flat = flat
myFlattenX (Elem a) flat = (flat ++ [a])
myFlattenX (List (x:xs)) flat = (myFlattenX x flat) ++ (myFlattenX (List xs) flat) 