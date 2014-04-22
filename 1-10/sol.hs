--Problem1
myLast (x:[]) = x
myLast (x:xs) = myLast xs

myLast' = foldr1 (flip const)

--Problem2
myButLast (x:_:[]) = x
myButLast (_:xs) = myButLast xs

--Problem3
elementAt (x:_) 1 = x
elementAt (_:xs) n 
            | n < 1 = error "Index out of bouds"
            | otherwise  = elementAt xs (n-1)
elementAt _ _ = error "Index out of bounds"

--Problem4
myLength = foldl (\n _ -> n+1) 0 

--Problem5
myReverse = foldl (flip (:)) []

--Problem6
isPalindrome xs = (reverse xs) == xs

isPalindrome' [] = True
isPalindrome' [_] = True
isPalindrome' xs = ((head xs) == (last xs)) && isPalindrome (tail$init$ xs) 

--Problem7
data NestedList a = Elem a | List [NestedList a]
flatten (Elem a) = [a]
flatten (List a) = concatMap flatten a

--Problem8
compress []  = [] 
compress str = compress'  (tail str) [(head str)]
    where compress' [] ls = ls
          compress' (x:xs) ls
            | x == (last ls) = compress' xs ls
            | otherwise = compress' xs (ls++[x])

--Problem9
span' f xs = helper ([] , xs)
        where 
           helper a = case a of
                        (as , []) -> a
                        (as , (x:bs)) -> if (f x) then helper (as ++ [x] , bs) else a 
pack [] = []
pack (x:xs) = (x:first) : (pack rest)
        where
            (first,rest) = span' (==x) xs
--Problem10
encode xs = map (\x->(length x , head x)) packed
    where packed = pack xs
 