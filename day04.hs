import System.IO
import Data.Array


type Input = Array (Int, Int) Char

parseInput :: String -> Input
parseInput ls =
  let rows = lines ls 
      rowcount = length rows
      colcount = length $ head rows
      bounds = ((1, 1), (rowcount, colcount))
  in listArray bounds (concat rows)


readInput :: FilePath -> IO Input
readInput path = 
  do inh <- openFile path ReadMode
     input <- hGetContents inh 
     return $ parseInput input 

-- We've read the input, now we nee to scan it.
-- Array bounds: ((1, 1), (N, N))
-- need to build range of indices for each point

-- Indices to check
surroundIdx (i, j) rows cols = filter goodIdx $ range ((i - 1, j - 1), (i + 1, j + 1))
  where goodIdx (r, c) = r > 0 && r <= rows && c > 0 && c <= cols && (r, c) /= (i, j)

-- Now each array element should be mapped to the amount of surrounding papers
countPapersAll :: Input -> Array (Int, Int) Int
countPapersAll input = array (bounds input) [ (i, countPapers input i) | i <- indices input]
  where countPapers input idx
          -- we count only for papery grid tiles, we ignore others
          | input ! idx == '@' =
            let (_, (rows, cols)) = bounds input 
                surroundChars = map (input !) (surroundIdx idx rows cols)
            in length $ filter (== '@') surroundChars
          | otherwise = 9

-- Finally we can count the number of elements which has < 4 rolls of paper
part1 input = length $ filter (< 4) $ elems $ countPapersAll input

runPart1 path = do input <- readInput path
                   return $ part1 input
