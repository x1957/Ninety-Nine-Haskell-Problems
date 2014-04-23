import Data.List
--Problem11
data StateX = Multiple Int Char | Single Char deriving (Show)
encodeModified xs = map helper grouped
           where
              grouped  = group xs
              helper x = let len = length x
                             ch = head x
                         in if len == 1 then Single ch else Multiple len ch
{-
    From solution
    encodeModified xs = [y | x <- group xs, let y = if (length x) == 1 then Single (head x) else Multiple (length x) (head x)]
-}
--Problem12
decodeModified = foldl helper ""
          where 
            helper acc (Multiple n ch) = acc ++ (take n $ repeat ch)
            helper acc (Single ch) = acc ++ [ch]

--Problem13
encodeDirect [] = []
encodeDirect xs = map helper' $ count' (1,head xs) (tail xs)
    where
        count' (n,ch) [] = [(n,ch)]
        count' (n,ch) (x:xs) = if x == ch then count' (n+1,ch) xs else (n,ch) : count' (1,x) xs
        helper' (1,ch) = Single ch
        helper' (n,ch) = Multiple n ch 

--Problem14
dupli = concatMap (\x->[x,x])

--Problem15
repli xs n = concatMap (replicate n) xs

--Problem16
dropEvery xs n = map fst $filter (\(ch,idx) -> idx `mod` n /= 0) $ zip xs [1..] --slow implement

--Problem17
split xs n = split' [] xs n
        where
            split' first rest 0 = (first , rest)
            split' first (x:rest) n = split' (first++[x]) rest (n-1)

--Problem18
slice xs start end = take (end-start+1) $ drop (start-1) xs

--Problem19
rotate xs n 
         | n < 0 = rotate xs (length xs + n)
         | otherwise = let (first , rest) = split xs n
                       in rest ++ first

--Problem20
removeAt n xs = removeAt' n [] xs
             where
                removeAt' 1 first (x:xs) = (x , first++xs)
                removeAt' n first (x:xs) = removeAt' (n-1) (first++[x]) xs
