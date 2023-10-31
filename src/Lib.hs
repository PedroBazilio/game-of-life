module Lib where

type Grid = [[Int]]
type Line = [Int]

-- >>>>>> LEITURA DA ENTRADA DE DADOS <<<<<< --

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


