import Data.Map
import Data.List

main = print $ "hi"


colList x = [(x, y) | y<-[0..8]]

rowList y = [(x, y) | x<-[0..8]]

threeFloor n = n - (n `mod` 3)

secRange coord = let n = threeFloor coord in [n..n+2]

secList x y = [(i, j)| i<-(secRange x), j<-(secRange y)]

peers x y = [tup | tup<-(nub $ colList x ++ rowList y ++ secList x y), tup /= (x,y)]

peerMap = fromList [((x, y), peers x y) | x<-[0..8], y<-[0..8]]
