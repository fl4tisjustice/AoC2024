import Data.List
import Text.Printf

getInput :: IO [[Int]]
getInput = do
    lists <- readFile "day-1/input.txt"
    let unzipped = map (map read . words) $ lines lists :: [[Int]]
    return . foldl (\(left:right:_) (first:second:_) -> [left ++ [first], right ++ [second]]) [[],[]] $ unzipped

partOne :: IO Int
partOne = do
    left:right:_ <- getInput
    return . sum . map abs $ zipWith (-) (sort left) (sort right)

count :: Eq a => a -> [a] -> Int
count elem = length . filter (== elem)

partTwo :: IO Int
partTwo = do
    left:right:_ <- getInput
    return . sum . zipWith (*) left . map (`count` right) $ left  

main :: IO ()
main = do
    partOneAnswer <- partOne
    partTwoAnswer <- partTwo
    printf "Part One: %d\nPart Two: %d\n" partOneAnswer partTwoAnswer
    