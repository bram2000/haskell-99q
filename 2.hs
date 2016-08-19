myButLast :: [a] -> a
myButLast [] = error "No penultimate item in an empty list!"
myButLast [x] = error "No penultimate item in a single item list!"
myButLast (x:xs) =
    if length (xs) == 1
        then x
        else myButLast (xs)
