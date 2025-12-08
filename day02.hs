import Data.Traversable
import System.IO
import Text.Parsec

-- Input parser
data IdRange = IdRange
        { from :: Integer
        , to :: Integer
        }
        deriving (Show)

rangeP :: (Stream s m Char) => ParsecT s u m IdRange
rangeP = do
        from <- many digit
        char '-'
        to <- many digit
        return $ IdRange (read from) (read to)

inputP :: (Stream s m Char) => ParsecT s u m [IdRange]
inputP = sepBy rangeP (char ',' >> spaces)

readInput :: FilePath -> IO [IdRange]
readInput path =
        do
                inh <- openFile path ReadMode
                input <- hGetContents inh
                case parse inputP path input of
                        Left e -> do
                                putStrLn "Error parsing input: "
                                print e
                                return []
                        Right r -> return r

-- Utility functions
sublist from to xs = take (to - from) $ drop from xs

{-
ID is invalid if it has
- even number of digits
- left half of ID is the same as right half of ID (sequence may repeat only twice)
-}
isInvalidId :: Integer -> Bool
isInvalidId x =
        let s = show x
            n = length s
         in if even $ length s
                then
                        let lx = sublist 0 (n `div` 2) s
                            rx = sublist (n `div` 2) n s
                         in lx == rx
                else
                        False

invalidIds :: (Integer -> Bool) -> IdRange -> [Integer]
invalidIds p (IdRange{from, to}) = filter p [from .. to]

part :: (Integer -> Bool) -> [IdRange] -> Integer
part p rs = sum $ concatMap (invalidIds p) rs

runPart1 path = do rs <- readInput path; return (part isInvalidId rs)

-- Part 2
isInvalidId2 :: Integer -> Bool
isInvalidId2 id =
        let s = show id
            n = length s
            ds = filter ((== 0) . (n `mod`)) [1 .. (n `div` 2)] -- Divisors
            gids = map (\a -> take n $ cycle $ take a s) ds -- Generated cycled ids
         in elem s gids

runPart2 path = do rs <- readInput path; return (part isInvalidId2 rs)
