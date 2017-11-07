import Data.Map as DM
import Data.List as DL
import Data.Maybe (fromJust, isJust)
import Control.Monad
import Data.Char as DC
import System.Environment

main = do
    args <- getArgs
    mapM_ putStrLn ((gprettify . fromJust . backtrack . greshape . stol) (head args))

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

empties :: [[Int]] -> [(Int, Int)]
empties grid = [(x, y) | x<-gRange, y<-gRange, getItem grid (x, y) == 0]

isLegal :: [[Int]] -> (Int, Int) -> Int -> Bool
isLegal grid (x, y) n = all (\k -> k /= n) [getItem grid (i, j) | (i, j)<-fromJust(DM.lookup (x, y) peerMap)]

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
    | DL.null $ empties grid = Just grid
    | DL.null $ legals grid (head (empties grid)) = Nothing
    | otherwise = msum (solList grid)

stol :: String -> [Int]
stol = DL.map DC.digitToInt

greshape :: [Int] -> [[Int]]
greshape [] = []
greshape l = take 9 l : greshape (drop 9 l)

gprettify :: [[Int]] -> [String]
gprettify grid = DL.map flatten grid
    where flatten = DL.map DC.intToDigit
