import Text.Parsec
import Data.Functor
import System.IO
import Data.List

{-
Input consists of:
- list of ranges
- list of numbers to check
-}
data IdRange = IdRange
        { from :: Integer
        , to :: Integer
        }
        deriving (Show)

type Input = ([IdRange], [Integer])

numberP :: Parsec String u Integer
numberP = many digit <&> read

rangeP :: Parsec String u IdRange
rangeP = do
  from <- numberP
  char '-'
  to <- numberP
  return $ IdRange from to 

inputP :: Parsec String u Input 
inputP = do
  ranges <- endBy1 rangeP endOfLine
  endOfLine
  ids <- sepBy1 numberP endOfLine
  eof
  return (ranges, ids)

readInput :: FilePath -> IO Input
readInput path = do
  inh <- openFile path ReadMode
  input <- hGetContents inh
  case parse inputP path input of
    Left e -> do
      putStrLn "Error parsing input: "
      print e
      error "bad input"
    Right r -> return r

-- Check that id is in range thus ingredient is fresh
inRange :: Integer -> IdRange -> Bool
inRange n (IdRange {from, to}) = n >= from && n <= to

containId :: [IdRange] -> Integer -> Bool
containId ranges n = any (inRange n) ranges 

part1 :: Input -> Int
part1 (ranges, ns) = length $ filter (containId ranges) ns
  
runPart1 path = do input <- readInput path
                   return $ part1 input

-- Part 2
{-
We need to make "union" of all ranges and calculate their size.
Possible way:
- sort all IdRanges in ascending order by "from"
- on each step check pair of ranges: if second's "from" is contained in first range - merge them, otherwise just add second range as independent one
-}
mergeRange [] nr = [nr]
mergeRange (r:rs) nr =
  if inRange (from nr) r
  then IdRange { from = from r, to = max (to r) (to nr) }:rs
  else nr:r:rs

-- Merge all ranges into smallest possible subset
mergeRanges :: [IdRange] -> [IdRange]
mergeRanges rs =
  -- order ranges in backward order by 'to' (because we use foldr)
  let ranges = sortBy (\a b -> compare (from b) (from a)) rs
  -- on each step merge next range to existing ones
  in reverse $ foldr (flip mergeRange) [] ranges

part2 = sum . map rangeLength . mergeRanges
  where rangeLength (IdRange from to) = to - from + 1

-- Load data, sort it in backward order (we are going foldr)
runPart2 path = do (rs, _) <- readInput path
                   return $ part2 rs
