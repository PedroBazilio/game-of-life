module Main (main) where
import GHC.Generics (prec)

-- 1 is alive, 2 is Dead and 3 is Zombie
data State = Dead | Alive | Zombie deriving (Eq, Show)
data Coord = Coord Integer Integer deriving (Eq, Show)

type Generation = Coord -> State
type Grid = [[Int]]
type Line = [Int]

-- Adicionar linha na matrix
addLine :: Grid -> Line -> IO Grid
addLine m linha = return (m ++ [linha])

-- Ler linha de numeros do terminal
getLinha :: Int -> Int -> IO Line
getLinha numCols n = do
    --putStrLn $ "Linha " ++ show n ++ ": "
    line <- getLine
    let nums = map read (words line) :: Line
    if length nums == numCols
        then return nums
        else if length nums > numCols
        then do
            let newNums = take numCols nums :: Line
            return newNums
        else if length nums < 3
            then do
                putStrLn "Entrada Invalida, digite novamentea."
                getLinha numCols n
            else return nums

-- Subtração
calSubtr :: Int -> Int
calSubtr n = n - 1

calSoma :: Int -> Int
calSoma n = n + 1

-- Construir Matriz
creatMatrix :: Int -> Int -> Grid -> Int -> IO Grid
creatMatrix numRows numCols matriz n = do
    if numRows <= 0
    then return matriz
    else do
        line <- getLinha numCols n
        matriz <- addLine matriz line
        creatMatrix (calSubtr numRows) numCols matriz (calSoma n)

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

  putStrLn " "
