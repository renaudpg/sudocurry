import Data.Map
import Data.List
import Data.Maybe (fromJust)

main = print $ --legals (replicate 9 (replicate 9 0)) (3, 3)

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

empties :: [[Int]] -> [(Int, Int)]
empties grid = [(x, y) | x<-gRange, y<-gRange, (getItem grid (x, y)) == 0]

isLegal :: [[Int]] -> (Int, Int) -> Int -> Bool
isLegal grid (x, y) n = all (\k -> k /= n) [getItem grid (i, j) | (i, j)<-fromJust(Data.Map.lookup (x, y) peerMap)]

legals :: [[Int]] -> (Int, Int) -> [Int]
legals grid (x, y) = [n | n<-[1..9], isLegal grid (x, y) n]

placeList :: [a] -> Int -> Int -> [a]
placeList xs pos val = [0] --todo

backtrack :: [[Int]] -> [[Int]]
backtrack grid
    | empties grid == [] = return grid
    | otherwise = [[0]] --todo
