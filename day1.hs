import System.IO
import System.Environment


-- Rotation is either left or right 
type Rotation = Either Integer Integer

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
rotate :: Integer -> Rotation -> Integer
rotate x (Left y)
  | y > 100 = rotate x (Left $ y `mod` 100)
  | x < y = x + (100 - y)
  | otherwise = x - y 
rotate x (Right y) = (x + y) `mod` 100

part1 :: [Rotation] -> Integer
part1 rs =
  let dialPositions = scanl rotate 50 rs
  in toInteger $ length $ filter (== 0) dialPositions

debug1 :: [Rotation] -> [(Rotation, Integer)]
debug1 rs =
  let dialPositions = scanl rotate 50 rs
   in zip rs $ tail dialPositions

main :: IO ()
main = do
  (a : as) <- getArgs
  rots <- readInput a 
  putStr "Part 1: "
  print $ part1 rots
