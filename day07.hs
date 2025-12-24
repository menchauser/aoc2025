import System.IO
import Data.Array.IArray
import Data.Array.MArray


-- To count amount of splits we need to count amount of splitters that were hit.
-- What could be suitable data structure?
-- An array? any (Int, Int) Char
type Input = Array (Int, Int) Char

-- Reads input with automatically inserted 0 column of '.'
parseInput s =
  let rows = map ('.':) $ lines s  -- Prepend each row with '.'
      rowcount = length rows
      colcount = length $ head rows
      bounds = ((1, 0), (rowcount, colcount))
  in listArray bounds (concat $ lines s)


readInput :: FilePath -> IO Input
readInput path =
  do inh <- openFile path ReadMode
     input <- hGetContents inh
     return $ parseInput input


-- TODO
myShow input = let es = elems input
                   (rows, cols) = bounds input
               in show rows ++ ":" ++ show cols


{-
-- Let's first calculate next step.
-- 
-- Input: (manifold, number of splits), row, col
-- Output: (character, new number of splits)

-- now we go from left to right 
-- by scanning square of previous elements. 
--}
beamStep :: (Input, Int) -> Int -> (Input, Int)
beamStep (ms, splits) row = undefined

{--
Calculates a character for given matrix position based on surroundings.
We are reviewing characters on positions 1-5 to calculate the dot position:
123
4.5

Possible outcomes:
_._  _|_  _^_  __|  |__  _S_
_._  _|_  _._  _|^  ^|_  _|_
--}
-- 
-- Idea
nextChar :: Input -> (Int, Int) -> Char
nextChar arr (r, c) = undefined
  -- first select surrounding chars
  -- then 

