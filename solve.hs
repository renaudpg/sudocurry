import Data.Map
import Data.List
import Data.Maybe (fromJust)

main = print $ firstEmpty (replicate 9 (replicate 9 0))

gRange = [0..8]


colList x = [(x, y) | y<-gRange]

rowList y = [(x, y) | x<-gRange]

threeFloor n = n - (n `mod` 3)

secRange coord = let n = threeFloor coord in [n..n+2]

secList x y = [(i, j)| i<-(secRange x), j<-(secRange y)]

peers x y = [tup | tup<-(nub $ colList x ++ rowList y ++ secList x y), tup /= (x,y)]

peerMap = fromList [((x, y), peers x y) | x<-gRange, y<-gRange]

head' :: [a] -> Maybe a
head' [] = Nothing
head' (x:xs) = Just x

getItem :: [[a]] -> (Int, Int) -> a
getItem xs (x, y) = xs!!y!!x

firstEmpty :: [[Int]] -> Maybe (Int, Int)
firstEmpty grid = head' [(x, y) | x<-gRange, y<-gRange, (getItem grid (x, y)) == 0]

isLegal :: [[Int]] -> (Int, Int) -> Int -> Bool
isLegal xs (x, y) n = all (\k -> k /= n) [getItem xs (i, j) | (i, j)<-fromJust(Data.Map.lookup (x, y) peerMap)]
