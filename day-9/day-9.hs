import Data.Char (digitToInt)
import Text.Printf (printf)

type DiskMap = [Block]
type CompressedDiskMap = [(Int, Block)]

data Block = Empty | File Int
    deriving (Eq)

instance Show Block where
    show block = case block of
        File id -> show id
        Empty -> "."

diskMap :: String -> DiskMap
diskMap input = fileBlock 0 $ map digitToInt input
    where
        fileBlock id (size:rest) = size `replicate` File id ++ emptyBlock (id + 1) rest
        fileBlock _ [] = []
        emptyBlock id (size:rest) =  size `replicate` Empty ++ fileBlock id rest
        emptyBlock _ [] = []

compress :: DiskMap -> CompressedDiskMap
compress [] = []
compress blocks@(block:_) = (length identical, block) : compress rest
    where (identical, rest) = span (==block) blocks


blockFragmenter :: CompressedDiskMap -> CompressedDiskMap
blockFragmenter compressed = go compressed $ reverse . filter (\x -> snd x /= Empty) $ compressed
    where go _ [last] = [last]
          go (file@(_, File _) : rest) toInsert = file : rest `go` init toInsert
          go ((emptyCount, _) : rest) toInsert =
            let (filesize, id) = head toInsert
            in (emptyCount `min` filesize, id) : case emptyCount `compare` filesize of
                LT -> rest `go` ((filesize - emptyCount, id) : tail toInsert)
                EQ -> rest `go` tail toInsert
                GT -> ((emptyCount - filesize, Empty) : rest) `go` tail toInsert

checksum :: CompressedDiskMap -> Int
checksum fragmented = go fragmented 0
    where go [] _ = 0 
          go ((count, File id) : rest) idx =
            sum (map (* id) [idx..idx + count - 1]) + go rest (idx + count)

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let compressed = compress . diskMap $ contents
        in printf "Part One: %d\n" . checksum . blockFragmenter $ compressed