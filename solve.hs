import Data.Map
import Data.List

main = print $ firstEmpty ((replicate 8 (replicate 9 1)) ++ [[1,0,1,1,1,1,1,1,1]])

gRange = [0..8]


colList x = [(x, y) | y<-gRange]

rowList y = [(x, y) | x<-gRange]

threeFloor n = n - (n `mod` 3)

secRange coord = let n = threeFloor coord in [n..n+2]

secList x y = [(i, j)| i<-(secRange x), j<-(secRange y)]

peers x y = [tup | tup<-(nub $ colList x ++ rowList y ++ secList x y), tup /= (x,y)]

peerMap = fromList [((x, y), peers x y) | x<-gRange, y<-gRange]


getItem :: [[a]] -> (Int, Int) -> a
getItem xs (x, y) = xs!!y!!x


firstEmpty grid = [(x, y) | x<-gRange, y<-gRange, (getItem grid (x, y)) == 0]
