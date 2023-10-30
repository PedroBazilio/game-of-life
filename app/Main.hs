-- module Main (main) where
import System.Random
import Control.Monad (replicateM)
import GHC.Generics (prec)

-- import Lib

-- main :: IO ()
-- main = someFunc
-- 1 is alive, 2 is Dead and 3 is Zombie
data State = Dead | Alive | Zombie deriving (Eq, Show)
data Coord = Coord Integer Integer deriving (Eq, Show)
type Generation = Coord -> State
type Grid = [[Int]]
type Line = [Int]

-- Verificações de estados
isAlive :: State -> Bool
isAlive Alive = True
isAlive Dead = False
isAlive Zombie = False

isDead :: State -> Bool
isDead Alive = False
isDead Dead = True
isDead Zombie = False

aliveNeighbors :: Generation -> Coord -> Int
aliveNeighbors generation coord = length (filter isAlive (map generation (neighbors coord)))

-- Gerando numeros de 1 a 3
randomValue :: IO Int
randomValue = randomRIO (1, 3)

-- -- Função para pegar vizinhos
neighbors :: Coord -> [Coord]
neighbors (Coord x y) =
  [ Coord (x-1) (y-1)
  , Coord x (y-1)
  , Coord (x+1) (y-1)
  , Coord (x+1) y
  , Coord (x+1) (y+1)
  , Coord x (y+1)
  , Coord (x-1) (y+1)
  , Coord (x-1) y
  ]


-- Criando matrizes com os tamanhos dados
generateRandomMatrix :: Int -> Int -> IO Grid
generateRandomMatrix numRows numCols = do
    let generateRow = replicateM numCols randomValue
    replicateM numRows generateRow

-- Adicionar linha na matrix
addLine :: Grid -> Line -> IO Grid
addLine m line = return (m ++ [line])

-- Ler linha de numeros do terminal
getLinha :: IO Line
getLinha = do
  line <- getLine
  let nums = map read (words line) :: [Int]
  return nums

-- Subtração
calSubtr :: Int -> IO Int
calSubtr n = return (n - 1)

-- Criar matriz
creatMatrix :: Int -> Grid -> IO Grid
creatMatrix numRows matriz = do
  if numRows <= 0
    then return matriz
    else do
      line <- getLinha
      matriz <- addLine matriz line
      numRows <- calSubtr numRows
      creatMatrix numRows matriz

main :: IO ()
main = do
    putStrLn "Enter the number of rows: "
    numRows <- readLn
    putStrLn "Enter the number of columns: "
    numCols <- readLn

    randomMatrix <- generateRandomMatrix numRows numCols

    putStrLn "Entre com as linhas da matriz:"
    putStrLn "OBS: utlize espaçamento entre os numeros e aperte enter apos digitar cada linha."
    m <- creatMatrix numRows []

    putStrLn "Random Matrix:"
    mapM_ print randomMatrix

    putStrLn "Leitura:"
    mapM_ print m
