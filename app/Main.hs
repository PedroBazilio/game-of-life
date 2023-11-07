{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Main where
import Text.Read (readMaybe)

-- 1 is Alive
-- 2 is Dead
-- 3 is Zombie

data Coord = Coord Int Int deriving (Eq, Show)
data StateNeighbors = StateNeighbors Int Int Int deriving (Eq, Show)
data Answer = Answer Grid Int deriving (Eq, Show)

type Cell = Int
type Row = [Int]
type Grid = [[Int]]
type NumCols = Int
type NumRows = Int

-- Função para pegar a matriz final do dado Answer
getGrid :: Answer -> Grid
getGrid (Answer grid _) = grid

-- Função para pergar o número de interações do dado Answer
getN :: Answer -> Int
getN (Answer _ num) = num

-- Adiciona linha na matrix
addRow :: Grid -> Row -> IO Grid
addRow grid row = return (grid ++ [row])

-- Verificar se existe algum erro no Input
inputErro :: [String] -> NumCols -> Bool
inputErro rowInput numCols = do
  not (any (< "1") rowInput || any (>"3") rowInput || (length rowInput < numCols))

-- Transforma o input em uma lista de String
inputToList :: String -> IO [String]
inputToList input = return (words input)

-- Ler uma linha de numeros do terminal
getRow :: NumCols -> IO Row
getRow numCols = do
  input <- getLine
  rowInput <- inputToList input
  if inputErro rowInput numCols
    then do
      let row = map read rowInput :: Row
      if length row == numCols
        then return row
        else if length row > numCols
          then do
            let newRow = take numCols row :: Row
            return newRow
          else return row
    else do
      putStrLn "Invalid Input. Try again."
      getRow numCols

-- Função para construir a matriz
createGrid :: NumRows -> NumCols -> Grid -> IO Grid
createGrid numRows numCols grid = do
    if numRows == 0
    then return grid
    else do
        row <- getRow numCols
        newGrid <- addRow grid row
        createGrid (numRows - 1) numCols newGrid

-- Função para calcular os index dos vizinhos de uma célula
neighbors :: Coord -> [Coord]
neighbors (Coord x y) =
  [ Coord x (y + 1)
  , Coord x (y - 1)
  , Coord (x + 1) y
  , Coord (x - 1) y
  , Coord (x - 1) (y - 1)
  , Coord (x + 1) (y - 1)
  , Coord (x - 1) (y + 1)
  , Coord (x + 1) (y + 1)
  ]

-- Função para testar de a célula está viva
isAlive :: Cell -> Bool
isAlive 1 = True
isAlive _ = False

-- Função para verificar se a celula é um zumbi
isZombie :: Cell -> Bool
isZombie 3 = True
isZombie _ = False

-- Função para verificar se a célula está morta
isDead :: Cell -> Bool
isDead 2 = True
isDead _ = False

-- Função para pegar o vizinho
getNeighbor :: Grid -> Coord -> NumRows -> NumCols -> IO Int
getNeighbor grid (Coord x y) numRows numCols = do
  if (x < numRows) && (y < numCols) && (x >= 0) && (y >= 0)
    then do
      return ((grid !! x) !! y)
    else return (-1)

-- Função para checar se o vizinho esta vivo, morto ou é zumbi
checkNeighbor :: Grid -> Coord -> NumRows -> NumCols -> StateNeighbors -> IO StateNeighbors
checkNeighbor grid (Coord x y) numRows numCols (StateNeighbors alive dead zombie) = do
  cell <- getNeighbor grid (Coord x y) numRows numCols
  if isAlive cell
    then return (StateNeighbors (alive + 1) dead zombie)
    else if isDead cell
      then return (StateNeighbors alive (dead + 1) zombie)
      else if isZombie cell
        then return (StateNeighbors alive dead (zombie + 1))
        else return (StateNeighbors alive dead zombie)

-- Regra se a celula estiver viva
ifAlive :: StateNeighbors -> Row -> IO Row
ifAlive (StateNeighbors alive _ zombie) row = do
  if zombie >= 1
    then return (row ++ [3])
    else if ((alive < 2) && (zombie <= 0)) || ((alive > 3) && (zombie <= 0))
      then return (row ++ [2])
      else return (row ++ [1])

-- Regra se a celula estiver morta
ifDead :: StateNeighbors -> Row -> IO Row
ifDead (StateNeighbors alive _ _) row = do
  if alive == 3
    then return (row ++ [1])
    else return (row ++ [2])

-- Regra se a celula for zombi
ifZombie :: StateNeighbors -> Row -> IO Row
ifZombie (StateNeighbors alive _ _) row = do
  if alive <= 0
    then return (row ++ [2])
    else return (row ++ [3])

-- Função para verificar a regra e descobrir qual será o estado da celula na resposta
checkStateNeighbors :: Cell -> Row -> StateNeighbors -> IO Row
checkStateNeighbors cell row (StateNeighbors alive dead zombie)
  | isAlive cell = ifAlive (StateNeighbors alive dead zombie) row
  | isDead cell = ifDead (StateNeighbors alive dead zombie) row
  | isZombie cell = ifZombie (StateNeighbors alive dead zombie) row
  | otherwise = return row

-- Função para dar inicio a verificações dos vizinhos
checkNeighbors :: Grid -> [Coord] -> NumRows -> NumCols -> StateNeighbors -> Int -> IO StateNeighbors
checkNeighbors grid coordsNeighbors numRows numCols stateNeighbors neighbor = do
  if neighbor < 8
    then do
      newStateNeighbors <- checkNeighbor grid (coordsNeighbors !! neighbor) numRows numCols stateNeighbors
      checkNeighbors grid coordsNeighbors numRows numCols newStateNeighbors (neighbor + 1)
    else return stateNeighbors

-- Função que verifica cada célula e inicia a verificação dos vizinhos
checkCell :: Grid -> Row -> Coord -> NumRows -> NumCols -> IO Row
checkCell grid row (Coord x y) numRows numCols = do
  if y < numCols
    then do
      stateNeighbors <- checkNeighbors grid (neighbors (Coord x y)) numRows numCols (StateNeighbors 0 0 0) 0
      newRow <- checkStateNeighbors ((grid !! x) !! y) row stateNeighbors
      checkCell grid newRow (Coord x (y+1)) numRows numCols
    else return row

-- Função que verifica a matriz
checkGrid :: Grid -> Grid -> Coord -> NumRows -> NumCols -> IO Grid
checkGrid grid newGrid (Coord x y) numRows numCols = do
  if x < numRows
    then do
      row <- checkCell grid [] (Coord x y) numRows numCols
      checkGrid grid (newGrid ++ [row]) (Coord (x + 1) y) numRows numCols
    else return newGrid

-- Função para verificar se todos os elementos de uma lista são iguais
allEqual :: Eq a => [a] -> Maybe a -> Bool
allEqual [] _ = True
allEqual (h:t) Nothing = allEqual t (Just h)
allEqual (h:t) (Just e)
    | h == e = allEqual t (Just e)
    | otherwise = False

-- Função para ler somente números
readNumber :: String -> IO Int
readNumber prompt = do
    putStr prompt
    input <- getLine
    case readMaybe input of
        Just num -> return num
        Nothing -> do
            putStrLn "Invalid input. Please enter a valid number."
            readNumber prompt

-- Função que inicia o jogo
startGame :: Grid -> Int -> Int -> NumRows -> NumCols -> IO Answer
startGame grid numInteractions counter numRows numCols = do
  if counter <= numInteractions
    then do
      answerGrid <- checkGrid grid [] (Coord 0 0) numRows numCols
      putStrLn $ "Interaction " ++ show counter ++ "."
      mapM_ print answerGrid
      putStrLn " "
      if allEqual (concat answerGrid) (Just 2)
        then return (Answer answerGrid counter)
        else do
          startGame answerGrid numInteractions (counter + 1) numRows numCols
    else do
      return (Answer grid numInteractions)

main :: IO ()
main = do

  putStrLn "\n"

  putStrLn "Number of interactions: "
  numInteractions <- readNumber ""

  putStrLn "\nNumber of rows in the matrix: "
  numRows <- readNumber ""

  putStrLn "Number of columns in the matrix: "
  numCols <- readNumber ""

  putStrLn " "

  putStrLn "Observation: Insert each line with numbers separated by spaces, \nas shown in the example."
  putStrLn "Example: 3x3 Matrix\n1 2 1\n2 3 1\n1 1 2\n"

  putStrLn "Start inserting the matrix: "
  grid <- createGrid numRows numCols []

  putStrLn "\n"

  putStrLn "Starting the Game of Life...\n"

  putStrLn (show numRows ++ "x" ++ show numCols ++" initial matrix:")
  mapM_ print grid

  putStrLn " "

  answerGrid <- startGame grid numInteractions 1 numRows numCols

  putStrLn ("Response Matrix after " ++ show (getN answerGrid) ++ " interactions:")
  mapM_ print (getGrid answerGrid)

  putStrLn " "

  putStrLn "Game of Life Finished!"

  putStrLn " "