import Data.Map as Map
import Data.List as List
import Data.Maybe (fromJust, isJust)
import Control.Monad
import Data.Char as Char
import System.Environment

type Coord = (Int, Int)
type Row = [Int]
type Sudoku = [Row]

main = do
    args <- getArgs
    mapM_ putStrLn ((format . fromJust . backtrack . reshape . parse) (head args))

range = [0..8]

peersInColumn x = [(x, y) | y<-range]

peersInRow y = [(x, y) | x<-range]

sectionIndex n = n - (n `mod` 3)

sectionRange coord = let n = sectionIndex coord in [n..n+2]

peersInSection x y = [(i, j)| i<-(sectionRange x), j<-(sectionRange y)]

peersOf x y = [tup | tup<-(nub $ peersInColumn x ++ peersInRow y ++ peersInSection x y), tup /= (x,y)]

peers = fromList [((x, y), peersOf x y) | x<-range, y<-range]

digitAt :: Sudoku -> Coord -> Int
digitAt xs (x, y) = xs!!y!!x

empties :: Sudoku -> [Coord]
empties sudoku = [(x, y) | x<-range, y<-range, digitAt sudoku (x, y) == 0]

isLegal :: Sudoku -> Coord -> Int -> Bool
isLegal sudoku (x, y) n = all (\k -> k /= n) [digitAt sudoku (i, j) | (i, j)<-fromJust(Map.lookup (x, y) peers)]

legalMovesAt :: Sudoku -> Coord -> [Int]
legalMovesAt sudoku (x, y) = [n | n<-[1..9], isLegal sudoku (x, y) n]

insertIn :: [a] -> Int -> a -> [a]
insertIn [] _ _ = []
insertIn (hd:tl) pos val
    | pos == 0  = val:tl
    | otherwise = hd:(insertIn tl (pos-1) val)

move :: Sudoku -> Coord -> Int -> Sudoku
move sudoku (x, y) val = insertIn sudoku y (insertIn (sudoku!!y) x val)

solutions :: Sudoku -> [Maybe Sudoku]
solutions sudoku = let empty = head (empties sudoku) in [backtrack (move sudoku empty val) | val <- (legalMovesAt sudoku empty)]

backtrack :: Sudoku -> Maybe Sudoku
backtrack sudoku
    | List.null $ empties sudoku = Just sudoku
    | List.null $ legalMovesAt sudoku (head (empties sudoku)) = Nothing
    | otherwise = msum (solutions sudoku)

parse :: String -> [Int]
parse = List.map Char.digitToInt

reshape :: [Int] -> Sudoku
reshape [] = []
reshape l = List.take 9 l : reshape (List.drop 9 l)

format :: Sudoku -> [String]
format sudoku = List.map flatten sudoku
    where flatten = List.map Char.intToDigit
