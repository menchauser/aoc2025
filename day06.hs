import Data.Either
import Data.Functor
import Data.List
import System.IO
import Text.Parsec
import Text.Read

-- Input is least of leasts of integers
-- and related list of operations

data Op = Add | Mul
        deriving (Show)

data Input = Input
        { problems :: [[Integer]]
        , operations :: [Op]
        }
        deriving (Show)

instance Read Op where
        readsPrec _ x =
                let (s, r) = span (/= ' ') x
                 in case s of
                        "*" -> [(Mul, r)]
                        "+" -> [(Add, r)]
                        c -> error $ "Cannot read Op from '" ++ c ++ "'"

testInput =
        Input
                { problems =
                        [ [123, 45, 6]
                        , [328, 64, 98]
                        , [51, 387, 215]
                        , [64, 23, 314]
                        ]
                , operations = [Mul, Add, Mul, Add]
                }

-- Read input: read list of ints followed by list of ops and transpose ints

-- Skip horizontal spaces (not newlines)
hspace :: Parsec String u ()
hspace = skipMany (oneOf " \t")

hspace1 :: Parsec String u ()
hspace1 = skipMany1 (oneOf " \t")

numListP :: Parsec String u [Integer]
numListP = do
  hspace
  many1 ((many1 digit <&> read) <* hspace)

opP = do
        c <- oneOf "*+"
        return $ read [c]

opListP :: Parsec String u [Op]
opListP = between hspace hspace $ endBy1 opP hspace

inputP :: Parsec String u Input
inputP = do
        rawProblems <- endBy1 numListP endOfLine
        ops <- opListP
        optional endOfLine
        eof
        return $ Input (transpose rawProblems) ops

readInput :: FilePath -> IO Input
readInput path = do 
  inh <- openFile path ReadMode
  input <- hGetContents inh
  case parse inputP path input of
    Left e -> do
      error $ "bad input: " ++ show e
    Right r -> return r

-- Part 1
calc :: Op -> [Integer] -> Integer
calc Mul numbers = product numbers
calc Add numbers = sum numbers

solve :: Input -> Integer
solve (Input ps ops) = sum $ zipWith calc ops ps

runPart1 path = do
  input <- readInput path
  return $ solve input


-- Part 2
{-
For Part 2 we read input data differently.
1. Read it into list of Strings: one for each input line.
2. Take all lines but last one (containing operations) and transpose them.
3. Transposed lines can be cleaned from whitespaces and split into groups. Split
  is done by the empty column separator between them.
4. Operations are parsed separately using Parsec.
-}

readInput2 path = do 
  inh <- openFile path ReadMode
  input <- hGetContents inh
  let ls = lines input
      plen = length ls - 1
      pls = take plen ls
      numstrings = map (filter (/= ' ')) $ transpose pls
      nums = (map (map read) $ split numstrings "") :: [[Integer]]
      opstring = head $ drop plen ls
      ops = fromRight [] $ parse opListP path opstring
    in return Input { problems = nums, operations = ops }

-- split xs into list of lists separated by sep
split :: (Eq a) => [a] -> a -> [[a]]
split [] sep = []
split xs sep = let (h, rest) = span (/= sep) xs
               in h : split (drop 1 rest) sep

runPart2 path = do
  input <- readInput2 path
  return $ solve input
