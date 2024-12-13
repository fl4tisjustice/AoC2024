import Text.Printf (printf)
import Data.Char (digitToInt)
import Data.Foldable (Foldable(toList))
import Data.Sequence (Seq, fromList, Seq((:<|), (:|>)), (><), (|>), (<|), spanl, singleton, viewl, breakl)
import qualified Data.Sequence as S (null, Seq(Empty), drop)

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
          emptyBlock ('0':rest) idx = fileBlock rest idx
          emptyBlock (count:rest) idx = (digitToInt count, Empty) : fileBlock rest idx

blockFragmenter :: CompressedDiskMap -> CompressedDiskMap
blockFragmenter compressed = go compressed $ reverse . filter ((/=Empty) . snd) $ compressed
    where go _ [last] = [last]
          go (file@(_, File _) : rest) toInsert = file : rest `go` init toInsert
          go ((emptyCount, _) : rest) toInsert =
            let (filesize, id) = head toInsert
            in (emptyCount `min` filesize, id) : case emptyCount `compare` filesize of
                LT -> rest `go` ((filesize - emptyCount, id) : tail toInsert)
                EQ -> rest `go` tail toInsert
                GT -> ((emptyCount - filesize, Empty) : rest) `go` tail toInsert

fileFragmenter :: CompressedDiskMap -> CompressedDiskMap
fileFragmenter = toList . go . fromList
    where go S.Empty = S.Empty
          go (occupied@(_, File _) :<| rest) = occupied <| go rest
          go (rest :|> empty@(_, Empty)) = go rest |> empty 
          go (search :|> file@(filesize, File _)) = 
            let (left, candidate) = spanl (\(size, block) -> size < filesize || block /= Empty) search
            in case candidate of
                S.Empty -> go search |> file
                ((size, Empty) :<| _) -> go (left >< singleton file >< leftover >< S.drop 1 candidate) |> (filesize, Empty)
                    where leftover = if filesize == size then S.Empty else singleton (size - filesize, Empty)

checksum :: CompressedDiskMap -> Int
checksum fragmented = go fragmented 0
    where go [] _ = 0
          go ((count, Empty) : rest) idx = go rest (idx + count)
          go ((count, File id) : rest) idx =
            (sum . map (* id)) [idx..idx + count - 1] + go rest (idx + count)

main :: IO ()
main = do
    compressed <- pure <$> compressedDiskMap =<< readFile "input.txt"
    printf "Part One: %d\nPart Two: %d\n" (checksum . blockFragmenter $ compressed) (checksum . fileFragmenter $ compressed)