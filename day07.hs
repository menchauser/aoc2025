import System.IO
import Data.Array


-- To count amount of splits we need to count amount of splitters that were hit.
-- What could be suitable data structure?
-- An array? and on each step scan one more line of array and add splitters.

type Input = Array (Int, Int) Char

parseInput s = 
  let rows = lines s 
      rowcount = length rows
      colcount = length $ head rows
      bounds = ((1, 1), (rowcount, colcount))
  in listArray bounds (concat $ lines s)

readInput :: FilePath -> IO Input
readInput path = 
  do inh <- openFile path ReadMode
     input <- hGetContents inh 
     return $ parseInput input 

-- TODO
myShow input = let es = elems input 
                   (rows, cols) = bounds input
               in (show rows) ++ ":" ++ (show cols)

-- Beam step
-- Input: (manifold, number of splits), row
-- Output: (manifold, new number of splits)
{-
On each step we need two lists:
- current row
- previous row

Scan crow by two chars from left to right. On each step:
- on ".^" - check if prow had "*|" on that place => produce "|^"
- on "^." - check if prow had "|*" on that place => produce "^|"
- on ".." - check "|*" or "*|" and produce "|." or "." accordingly

Or maybe we can go array wise
on each step we have
((r, c), '.') =>
- check ((r - 1, c), '|')
- check ((r - 1, c + 1), '|') && ((r, c + 1), '^')
         || ((r - 1, c - 1), '|') && ((r, c - 1), '^')
((r, c), '^') => check ((r - 1, c), '|')
but how to update left-right coordinates then?
what is the general formula of a row value
-}
beamStep :: (Input, Int) -> Int -> (Input, Int)
beamStep (ms, splits) row =

