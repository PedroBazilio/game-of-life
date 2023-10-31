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

isZombie :: State -> Bool
isZombie Alive = False
isZombie Dead = False
isZombie Zombie = True

aliveNeighbors :: Generation -> Coord -> Int
aliveNeighbors generation coord = length (filter isAlive (map generation (neighbors coord)))

deadNeighbors :: Generation -> Coord -> Int
deadNeighbors generation coord = length (filter isDead (map generation (neighbors coord)))


zombieNeighbors :: Generation -> Coord -> Int
zombieNeighbors generation coord = length (filter isZombie (map generation (neighbors coord)))


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

    putStrLn "Entre com as linhas da matriz:"
    putStrLn "OBS: utlize espaçamento entre os numeros e aperte enter apos digitar cada linha."
    m <- creatMatrix numRows []

    putStrLn "Leitura:"
    mapM_ print m