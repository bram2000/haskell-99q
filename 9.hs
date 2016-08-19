module MyPack where

pack :: Eq a => [a] -> [[a]]

pack []  = []
pack [x] = [[x]]
pack (x:y:xs) = if x == y
	then (x:(head (pack (y:xs)))):(tail (pack (y:xs)))
	else [x]:(pack (y:xs))


--fun :: [a] -> [[a]]
--
--fun [] = []
--fun [x] = [[x]]
--fun (x:xs) = [x:(head (fun xs))]
