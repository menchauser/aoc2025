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

numListP :: Parsec String u [Integer]
numListP = do
  optional hspace -- Line can start with spaces
  sepBy1 (many1 digit <&> read) hspace

opP = do
        c <- oneOf "*+"
        return $ read [c]

opListP :: Parsec String u [Op]
opListP = endBy1 opP hspace

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
