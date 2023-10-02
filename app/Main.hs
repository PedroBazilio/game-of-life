-- module Main (main) where
import System.Random
import Control.Monad (replicateM)

-- import Lib

-- main :: IO ()
-- main = someFunc
-- 1 is alive, 2 is Dead and 3 is Zombie
data State = Dead | Alive | Zombie deriving (Eq, Show)
data Position = Position Integer Integer
type Generation = Position -> State
type Grid = [[Int]]



-- Generating numbers from 1 to 3
randomValue :: IO Int
randomValue = randomRIO (1, 3)

-- Creating a matrix from the giving sizes
generateRandomMatrix :: Int -> Int -> IO Grid
generateRandomMatrix numRows numCols = do
    let generateRow = replicateM numCols randomValue
    replicateM numRows generateRow

main :: IO ()
main = do
    putStrLn "Enter the number of rows: "
    numRows <- readLn
    putStrLn "Enter the number of columns: "
    numCols <- readLn

    randomMatrix <- generateRandomMatrix numRows numCols

    putStrLn "Random Matrix:"
    mapM_ print randomMatrix
