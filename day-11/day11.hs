import GHC.Utils.Misc (split)
import qualified Data.IntMultiSet as IMS (fromList, concatMap, size)

getInput :: IO [Int]
getInput =  map read . split ' ' <$> readFile "day-11/input.txt" :: IO [Int]

blink :: Int -> [Int]
blink stone
    | stone == 0 = [1]
    | even . length $ stoneStr = map read [take half stoneStr, drop half stoneStr]
    | otherwise = [2024 * stone]
    where stoneStr = show stone
          half = length stoneStr `div` 2

-- Part 2 based on https://work.njae.me.uk/2024/12/11/advent-of-code-2024-day-11/
-- (Incidentally my part 1 implementation was virtually identical)
-- Was not aware of the "infamous" Lanternfish example from a previous year
-- as this is my first time going so far into AoC while it's running
-- All of this to say skill issue will do better next time

-- From what I understand, using a multiset overcomes the big problem
-- the naive version of the solution has, which is having to hold the
-- whole list in memory; a list whose size increases exponentially with
-- each iteration, and, consequently, so does the time to traverse it

-- Using a multiset lets us represent any amount of identical elements as
-- a pair of `Int`s corresponding to the value and amount of occurrences,
-- and so not only is iterating through the elements is much less expensive

-- A further possible optimization could be caching previous computations,
-- i.e. DP memoisation due to overlapping subproblems


blinkN :: Int -> [Int] -> Int
blinkN times stones = IMS.size $ (!! times) $ IMS.concatMap blink `iterate` IMS.fromList stones

partOne :: [Int] -> Int
partOne = blinkN 25

partTwo :: [Int] -> Int
partTwo = blinkN 75

main :: IO ()
main = do
    stones <- getInput
    print . partTwo $ stones