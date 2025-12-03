import System.IO
import System.Environment


-- Rotation is either left or right 
type Rotation = Either Int Int

parseRotation :: String -> Rotation
parseRotation ('L' : cs) = Left (read cs)
parseRotation ('R' : cs) = Right (read cs)

parseInput :: String -> IO [Rotation]
parseInput input = pure $ map parseRotation $ lines input

readInput :: FilePath -> IO [Rotation]
readInput path = 
  do inh <- openFile path ReadMode
     input <- hGetContents inh 
     parseInput input 

-- Now we have list of rotations and need to count number of zeros 
rotate :: Int -> Rotation -> Int
rotate x (Left y)
  | y > 100 = rotate x (Left $ y `mod` 100)
  | x < y = x + (100 - y)
  | otherwise = x - y 
rotate x (Right y) = (x + y) `mod` 100

part1 :: [Rotation] -> Int
part1 rs =
  let dialPositions = scanl rotate 50 rs
  in length $ filter (== 0) dialPositions

debug1 :: [Rotation] -> [(Rotation, Int)]
debug1 rs =
  let dialPositions = scanl rotate 50 rs
   in zip rs $ tail dialPositions

-- Part 2
-- If during rotation we got value bigger than 100 or less or equal to 0 - it is a click
rotate2 :: (Int, Int) -> Rotation -> (Int, Int)
rotate2 (state, clicks) (Left n) =
  let newState = state - n
  in (newState `mod` 100, clicks + abs (newState `div` 100))
rotate2 (state, clicks) (Right n) = 
    let newState = state + n
    in (newState `mod` 100, clicks + abs (newState `div` 100))

part2 :: [Rotation] -> Int
part2 rs =
  let dialPositions = scanl rotate 50 rs
  in length $ filter (== 0) dialPositions

main :: IO ()
main = do
  (a : as) <- getArgs
  rots <- readInput a 
  putStr "Part 1: "
  print $ part1 rots
