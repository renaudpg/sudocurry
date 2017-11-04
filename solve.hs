import Data.Map
import Data.List
import Data.Maybe (fromJust, isJust)

main = print $ let g = [[6,4,1,8,5,9,3,2,7],[7,2,9,4,3,6,8,1,5],[5,3,8,2,7,1,6,4,9],[9,5,6,1,2,3,7,8,4],[4,7,2,6,8,5,9,3,1],[8,1,3,9,4,7,2,5,6],[2,8,5,7,9,4,1,0,3],[3,6,7,5,1,2,4,9,8],[1,9,4,0,6,8,5,7,2]] in backtrack g

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
empties grid = [(x, y) | x<-gRange, y<-gRange, getItem grid (x, y) == 0]

isLegal :: [[Int]] -> (Int, Int) -> Int -> Bool
isLegal grid (x, y) n = all (\k -> k /= n) [getItem grid (i, j) | (i, j)<-fromJust(Data.Map.lookup (x, y) peerMap)]

legals :: [[Int]] -> (Int, Int) -> [Int]
legals grid (x, y) = [n | n<-[1..9], isLegal grid (x, y) n]

placeList :: [a] -> Int -> a -> [a]
placeList [] _ _ = []
placeList (hd:tl) pos val
    | pos == 0  = val:tl
    | otherwise = hd:(placeList tl (pos-1) val)

placeGrid :: [[a]] -> (Int, Int) -> a -> [[a]]
placeGrid grid (x, y) val = placeList grid y (placeList (grid!!y) x val)

solList :: [[Int]] -> [Maybe [[Int]]]
solList grid = let fe = head (empties grid) in [backtrack (placeGrid grid fe val) | val <- (legals grid fe)]

backtrack :: [[Int]] -> Maybe [[Int]]
backtrack grid
    | Data.List.null $ empties grid = Just grid
    | Data.List.null $ legals grid (head (empties grid)) = Nothing
    | otherwise = fromJust (find (\g -> isJust g) (solList grid))
