-- module Main (main) where
import Lib
import GHC.Generics (prec)

import System.Random
import Control.Monad (replicateM)

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

main :: IO ()
main = do
  putStrLn " "
  putStrLn "Numero de linhas: "
  numRows <- readLn

  putStrLn "Numero de colunas: "
  numCols <- readLn

  putStrLn " "

  putStrLn "OBS: Insera cada linha de forma verifical\ne os numeros separados por espaço."
  putStrLn "Ex:\n1 2 1\n2 3 1\n1 1 2\n"
  putStrLn "Insira os dados da matriz: "
  m <- creatMatrix numRows numCols [] 1
  putStrLn " "
  putStrLn "Matriz de Entrada:"
  mapM_ print m

  randomMatrix <- generateRandomMatrix numRows numCols
  putStrLn " "
  putStrLn "Random Matrix:"
  mapM_ print randomMatrix

  putStrLn " "
