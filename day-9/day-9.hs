import Data.Char (digitToInt)
import Text.Printf (printf)

type CompressedDiskMap = [(Int, Block)]

data Block = Empty | File Int
    deriving (Eq)

instance Show Block where
    show block = case block of
        File id -> show id
        Empty -> "."

compressedDiskMap :: String -> CompressedDiskMap
compressedDiskMap input = fileBlock input 0
    where fileBlock [] _ = []
          fileBlock (count:rest) idx = (digitToInt count, File idx) : emptyBlock rest (idx + 1)
          emptyBlock [] _ = []
          emptyBlock (count:rest) idx = (digitToInt count, Empty) : fileBlock rest idx

blockFragmenter :: CompressedDiskMap -> CompressedDiskMap
blockFragmenter compressed = go compressed $ reverse . filter ((/=Empty) . uncurry seq) $ compressed
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
            (sum . map (* id)) [idx..idx + count - 1] + go rest (idx + count)

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let compressed = compressedDiskMap contents
        in printf "Part One: %d\n" . checksum . blockFragmenter $ compressed