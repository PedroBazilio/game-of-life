-- module Main (main) where
import System.Random
import Control.Monad (replicateM)

-- import Lib

-- main :: IO ()
-- main = someFunc
-- 1 is alive, 2 is Dead and 3 is Zombie
data State = Dead | Alive | Zombie deriving (Eq, Show)
data Coord = Coord Integer Integer deriving (Eq, Show)
type Generation = Coord -> State
type Grid = [[Int]]



-- Gerando numeros de 1 a 3
randomValue :: IO Int
randomValue = randomRIO (1, 3)

-- Função para verificar se é vizinho
adjacent :: Coord -> Coord -> Bool
adjacent p q | p == q = False
adjacent (Coord x1 y1) (Coord x2 y2) = abs (x1 - x2) <= 1 && abs (y1 - y2) <= 1

-- Criando matrizes com os tamanhos dados
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
