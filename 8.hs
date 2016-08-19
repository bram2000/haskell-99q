import Debug.Hood.Observe

compress :: (Eq a, Observable a) => [a] -> [a]
compress' :: (Eq a, Observable a) => [a] -> [a]

compress []  = []
compress [x] = [x]
compress (x:xs) = if x == head xs
	then compress xs
	else [x] ++ compress xs

compress' = observe "Blah" compress
