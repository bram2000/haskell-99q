elementAt :: [a] -> Integer -> a
elementAt [] _ = error "Empty list!"
elementAt [x] 1 = x
elementAt [x] _ = error "Invalid index!"
elementAt (x:xs) 1 = x
elementAt (x:xs) y = elementAt xs (y - 1)
