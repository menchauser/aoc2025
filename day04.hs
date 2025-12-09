import System.IO
import Data.Array


type Input = Array (Int, Int) Int 

-- Input is parsed into array of 0 and 1. 0 - floor tile, 1 - paper.
parseInput :: String -> Input
parseInput ls =
  let rows = lines ls 
      rowcount = length rows
      colcount = length $ head rows
      bounds = ((1, 1), (rowcount, colcount))
      arr_data = map (\c -> if c == '@' then 1 else 0) (concat rows)
  in listArray bounds arr_data


readInput :: FilePath -> IO Input
readInput path = 
  do inh <- openFile path ReadMode
     input <- hGetContents inh 
     return $ parseInput input 

-- We've read the input, now we nee to scan it.
-- Array bounds: ((1, 1), (N, N))
-- need to build range of indices for each point

-- Indices to check for each grid position
surroundIdx (i, j) rows cols = filter goodIdx $ range ((i - 1, j - 1), (i + 1, j + 1))
  where goodIdx (r, c) = r > 0 && r <= rows && c > 0 && c <= cols && (r, c) /= (i, j)

-- Count amount of papers in array
totalPaper :: Input -> Integer 
totalPaper = sum . map toInteger . elems

-- Do one step of paper removal
removePaper :: Input -> Input
removePaper input = array (bounds input) [ (i, removePaper' input i) | i <- indices input ]
  where removePaper' input i
          -- For paper tile - calculate new value
          | input ! i == 1 =
              let (_, (rows, cols)) = bounds input
                  surroundPapers = sum $ map (input !) (surroundIdx i rows cols)
              in if surroundPapers < 4 then 0 else 1
          -- Skip non-paper tiles
          | otherwise = 0

-- Part 1: Count the number of papers removed in one step 
part1 input = let currentPaper = totalPaper input 
                  nextPaper = totalPaper $ removePaper input
              in currentPaper - nextPaper

runPart1 path = do input <- readInput path
                   return $ part1 input

-- Part 2 Count total possible amount of paper removed
part2 :: Input -> Integer
part2 input =
  -- All future floor states: with paper removed on each step
  let floorStates = iterate removePaper input 
      -- Counts of paper on each step
      paperCounts = map totalPaper floorStates
      -- How many papers were removed on each step
      paperCountDiffs = zipWith (-) paperCounts (tail paperCounts)
      -- Summarize total amount of paper removed 
  in sum $ takeWhile (> 0) paperCountDiffs 

runPart2 path = do input <- readInput path
                   return $ part2 input
