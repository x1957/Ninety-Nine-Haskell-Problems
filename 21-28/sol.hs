import System.Random
import Control.Monad (replicateM)
import Data.List
--Problem21
insertAt ch str pos = insertAt' ch [] str pos
           where 
              insertAt' ch first rest 1 = first ++ [ch] ++ rest
              insertAt' ch first (x:rest) n = insertAt' ch (first++[x]) rest (n-1)

--Problem22
range start end = [start..end]

--Problem23
rnd_select xs n = do
    gen <- getStdGen
    return $ take n [xs !! x | x <- randomRs (0 , (length xs) - 1) gen]

--Problem24
diff_select n m = do
    gen <- getStdGen
    return $ take n $ nub $ randomRs (1,m) gen

--Problem25
rnd_permu xs = do
    idx <- diff_select (length xs) (length xs)
    return [xs !! (i-1) | i<-idx]

--Problem26
combinations 0 _ = [[]]
combinations _ []  = []
combinations n (x:xs) = [x:y | y <- combinations (n-1) xs] ++ combinations n xs

--Problem27 