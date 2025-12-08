This is an implementation of Day 3 of Advent of Code


> import System.IO
> import Data.Maybe
> import Data.List

We consider input as mere list of strings (lines)

> type Input = [String]
> readInput :: FilePath -> IO Input
> readInput path =
>   do inh <- openFile path ReadMode
>      input <- hGetContents inh
>      return $ lines input

Naive solution to find largest possible joltage in bank:
- For each battery (up to penultimate one) find largest joltage right from it in the bank.
- Calculate the maximum joltage amongst ones derived on previous step.
- For each subbank (all batteries from first, all batteries from second, etc): calculate maximum
  joltage and then find aggregate maximum

> maxJoltage :: String -> Int
> maxJoltage cs = let remBanks = take (length cs - 1) $ iterate tail cs in
>   maximum $ map maxJoltage' remBanks
>   where maxJoltage' (c:cs) = read [c, maximum cs]

The solution and its runner

> part :: (String -> Integer) -> Input -> Integer
> part jfunc lines = sum $ map jfunc lines

> runPart :: (String -> Integer) -> FilePath -> IO Integer
> runPart jfunc path = do
>   input <- readInput path
>   return $ part jfunc input


For second part we need to use 12 batteries in bank.
The idea could be like this. We need to select 12 batteries out of N which would comprise together
maximum joltage. Possible imperative solution is based on principle: on each step we should select
battery of biggest possible joltage amongst once remaining available to us. Batteries remaining
available to us are located starting right from the current battery till the (L - (N - n - 1)),
where:
- L - total bank size
- N - required amount of batteries
- n - currently selected amount of batteries.

For example, consider bank 818181911112111. We need to select N=12 batteries out of L=15.
For starters, we select first biggest battery from first one to the (15 - (12 - 0 - 1)) = 4th one
F(inclusive). On second step we select biggest one from the position of first selected to the
(15 - (12 - 1 - 1)) = 5th one. Illustrating:

Ids:   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15
Bank:  8  1  8  1  8  1  9  1  1  1  1  2  1  1  1
From:  8  1  8  1
Max:   8
From:     1  8  1  8
Max:         8
From:           1  8  1
Max:               8
From:                 1  9
Max:                     9
From:                       1
Max:                        1
etc

This we get:
       8     8     8     9  1  1   1  1  2  1  1  1


In Haskell, we can go with itereration like this:

> maxJoltageAcc acc 0 _ = reverse acc
> maxJoltageAcc acc n remBank =
>   let pool_size = length remBank - n + 1 -- allowed number of batteries to select from
>       (b, idx) = maxIndex (take pool_size remBank)
>   in maxJoltageAcc (b:acc) (n - 1) (drop (idx + 1) remBank)
>   where
>     maxIndex xs = let x = maximum xs
>                   in (x, fromJust (elemIndex x xs))

> maxJoltage2 :: String -> Integer
> maxJoltage2 s = read $ maxJoltageAcc "" 12 s


To call solutions:

runPart maxJoltage "test03.input"
runPart maxJoltage2 "test03.input"
