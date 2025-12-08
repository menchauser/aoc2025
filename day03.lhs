This is an implementation of Day 3 of Advent of Code

> import System.IO
> import Text.Parsec

We can consider input as mere list of strings (lines)

> type Input = [String]
> readInput :: FilePath -> IO Input
> readInput path =
>   do inh <- openFile path ReadMode
>      input <- hGetContents inh
>      return $ lines input 

Naive solution to find largest possible joltage in bank:
- For each battery (up to penultimate one) find largest joltage right from it in the bank.
- Calculate the maximum joltage amongst ones derived on previous step.
- For each subbank (all batteries from first, all batteries from second, etc): calculate maximum joltage and then find aggregate maximum 

> maxJoltage :: String -> Int
> maxJoltage cs = let remBanks = take (length cs - 1) $ iterate tail cs in
>   maximum $ map maxJoltage' remBanks
>   where maxJoltage' (c:cs) = read [c, maximum cs]

The solution and its runner

> part1 :: Input -> Int
> part1 lines = sum $ map maxJoltage lines 

> runPart1 :: FilePath -> IO Int
> runPart1 path = do
>   input <- readInput path
>   return $ part1 input 
