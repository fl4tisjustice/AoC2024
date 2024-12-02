import Data.List    (sort, nub, isInfixOf, findIndex)
import Data.Ix      (inRange, range)
import Text.Printf  (printf)

getInput :: IO [[Int]]
getInput = do
    lists <- readFile "day-2/input.txt"
    return . map (map read . words) $ lines lists :: IO [[Int]]

monotonic :: [Int] -> Bool
monotonic list =
    let cmp = sort . nub $ list
    in (((list `isInfixOf`) `any` [cmp, reverse cmp]) && (inRange (1,3) . maximum . map abs $ zipWith (-) list $ tail list))

partOne :: IO Int
partOne = sum . map (fromEnum . monotonic) <$> getInput

partTwo :: IO Int
partTwo = do
    let deleteNth n xs = let (a, b) = splitAt n xs in a ++ tail b
        in sum . map (\report ->
            fromEnum $ monotonic report ||
            any (monotonic . (`deleteNth` report)) (range (0, length report - 1))
        ) <$> getInput


main :: IO ()
main = partOne >>= printf "Part One: %d\n" >> partTwo >>= printf "Part Two: %d\n"