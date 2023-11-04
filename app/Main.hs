module Main (main) where
import GHC.Generics (prec)

-- 1 is alive, 2 is Dead and 3 is Zombie
data Coord = Coord Int Int deriving (Eq, Show)
-- V alive, dead, zombie (Utilizado para ir somando o numero total de vizinhos, mortos, vivos e zumbis)
data V = V Int Int Int deriving (Eq, Show)

type Line = [Int]
type Grid = [[Int]]

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
        else if length nums < numCols
            then do
                putStrLn "Entrada Invalida, digite novamentea."
                getLinha numCols n
            else return nums

-- Construir Matriz
creatMatrix :: Int -> Int -> Grid -> Int -> IO Grid
creatMatrix numRows numCols matriz n = do
    if numRows <= 0
    then return matriz
    else do
        line <- getLinha numCols n
        matriz <- addLine matriz line
        creatMatrix (numRows - 1) numCols matriz (n + 1)

vizinhos :: Coord -> [Coord]
vizinhos (Coord x y) =
    [ Coord x (y + 1)
    , Coord x (y - 1)
    , Coord (x + 1) y
    , Coord (x - 1) y
    , Coord (x - 1) (y - 1)
    , Coord (x + 1) (y - 1)
    , Coord (x - 1) (y + 1)
    , Coord (x + 1) (y + 1)
    ]

isAlive :: Int -> Bool
isAlive 1 = True
isAlive _ = False

isZombie :: Int -> Bool
isZombie 3 = True
isZombie _ = False

isDead :: Int -> Bool
isDead 2 = True
isDead _ = False

-- Função que retorna o vizinho caso exista
-- Entrada: Matriz de Entrada, Coordada que deseja ler, numero de linhas e numeros de colunas
getVizinho :: Grid -> Coord -> Int -> Int -> IO Int
getVizinho m (Coord x y) numRows numCols = do
    if (x < numRows) && (y < numCols) && (x >= 0) && (y >= 0)
        then return ((m !! x) !! y)
        else return (-1)

-- Função que faz a verificação se o vizinho é vivo, morto ou zumbi
-- Entrada: Matriz de Entrada, Coordernada do Vinho, Numero de linhas, Numero de colunas e V
-- Saida: V
verificaVizinho :: Grid -> Coord -> Int -> Int -> V -> IO V
verificaVizinho m (Coord x y) numRows numCols (V a d z) = do
    r <- getVizinho m (Coord x y) numRows numCols
    if isAlive r
        then return (V (a+1) d z)
        else if isDead r
            then return (V a (d + 1) z)
            else if isZombie r
                then return (V a d (z + 1))
                else return (V a d z)

-- Função que verifica o resultado final de uma celula, se vai ficar 
--          viva, morte, ou zumbi na proxima interação ou saida
-- Entrada: Celula que esta sendo verificada, Linha de Resposta, V 
-- Saida: Linha de resposta da matriz de resposta
verificaResultado :: Int -> Line -> V -> IO Line
verificaResultado c rowResp (V a d z)
    | isAlive c = if z >= 1
        then return (rowResp ++ [3])
        else (if ((a < 2) && (z <= 0)) || ((a > 3) && (z <= 0))
            then return (rowResp ++ [2])
            else return (rowResp ++ [1]))
    | isDead c = if a == 3
            then return (rowResp ++ [1])
            else return (rowResp ++ [2])
    | isZombie c = if a <= 0
            then return (rowResp ++ [2])
            else return (rowResp ++ [3])
    | otherwise = return rowResp

-- Função recursiva que verifica os 8 vizinhos possiveis da celula desejada.
-- Entrada: Matriz de Entrada, Lista de coordenadas dos vizinhos para verificar,
--          Numero de linhas, Numero de colunas, v, numero do viziho que esta olhando
-- Saida: Dado (V a d z), onde a é o numero de vizinhos vivos, 
--        d o numero de mortos e z o numero de zumbis
verificaVizinhos :: Grid -> [Coord] -> Int -> Int -> V -> Int -> IO V
verificaVizinhos m coords numRows numCols v numV = do
    if numV < 8
        then do
            p <- verificaVizinho m (coords !! numV) numRows numCols v
            verificaVizinhos m coords numRows numCols p (numV + 1)
        else return v

-- Função recursa que verifica cada elemente de uma linha da matriz
-- Entrada: Matriz Entrada, Linha da Matriz de Resposta,
--          Coordernada que sera verificada (Sempre só mudara o y), Numero de Linhas
--          Numero de colunas
-- Saida: Linha x completa da matriz de entrada depois das verificações
verificaColuna :: Grid -> Line -> Coord -> Int -> Int -> IO Line
verificaColuna m rowResp (Coord x y) numRows numCols = do
    if y < numCols
        then do
            v <- verificaVizinhos m (vizinhos (Coord x y)) numRows numCols (V 0 0 0) 0
            resp <- verificaResultado ((m !! x) !! y) rowResp v
            verificaColuna m resp (Coord x (y+1)) numRows numCols
        else return rowResp

-- Função recursiva que verifica a matriz linha por linha
-- Entrada: Matriz de Entrada, Matriz Resposta, 
--          Cordenada da Linha (Sempre será -> Coord [Alguma coisa] 0),
--          Numero de Linhas, Numero de colunas
-- Saida: A matriz totalmente resolvida após uma unica interação
verificaMatriz :: Grid -> Grid -> Coord -> Int -> Int -> IO Grid
verificaMatriz m mSaida (Coord x y) numRows numCols = do
    if x < numRows
        then do
            l <- verificaColuna m [] (Coord x y) numRows numCols
            verificaMatriz m (mSaida ++ [l]) (Coord (x + 1) y) numRows numCols
        else return mSaida

-- Função que inicia o Jogo
-- Entradas: Matriz Inicial, Matriz Resposta Anterior, 
--          Numero de Interações, Numero de linhas, Numero de colunas
-- Saida: Matriz totalmente resolvida apos n interações

-- startGame :: Grid -> Grid -> Int -> Int -> Int -> IO Grid
-- startGame m mResp n numRows numCols = do
--     if (n > 0) && (m /= mResp)
--         then do
--             resp <- verificaMatriz m [] (Coord 0 0) numRows numCols
--             startGame resp m (n-1) numRows numCols
--         else return m

startGame :: Grid -> Grid -> Int -> Int -> Int -> Int -> IO ()
startGame m mResp n numRows numCols currentIter = do
    if (currentIter <= n)
        then do
            putStrLn $ "Iteração " ++ show currentIter ++ ":"
            mResp <- verificaMatriz m [] (Coord 0 0) numRows numCols
            mapM_ print mResp
            putStrLn " "

            -- Checa estabilidade
            if mResp == m
                then do
                    putStrLn "System estabilizado."
                    putStrLn $ "Número de passos até estabilizar: " ++ show currentIter
                else
                    startGame mResp mResp n numRows numCols (currentIter + 1)
        else return ()


mExemplo :: Grid
mExemplo =
  [
  [1, 3, 2, 1, 3, 2],
  [3, 1, 2, 3, 1, 1],
  [1, 1, 1, 2, 1, 2],
  [3, 1, 3, 2, 1, 1],
  [1, 1, 1, 2, 1, 2],
  [3, 2, 2, 1, 3, 3]
  ]

main :: IO ()
main = do

    putStrLn " "

    putStrLn "Matriz de Exemplo:"
    mapM_ print mExemplo

    putStrLn "\n"

    putStrLn "Numero de interacoes: "
    numInt <- readLn

    putStrLn " "
    putStrLn "Iniciando Jogo da Vida..."

    startGame mExemplo mExemplo numInt 6 6 1

    putStrLn "Jogo da Vida finalizado!."

    -- COMENTAR PARA NÃO PRECISAR FICAR COLOCANDO OS DADOS TODA HORA

    putStrLn " "

    --putStrLn "Numero de linhas: "
    --numRows <- readLn

    --putStrLn "Numero de colunas: "
    --numCols <- readLn



    --putStrLn " "

    --putStrLn "OBS: Insera cada linha de forma verifical\ne os numeros separados por espaço."
    --putStrLn "Ex:\n1 2 1\n2 3 1\n1 1 2\n"

    --putStrLn "Insira os dados da matriz: "
    --m <- creatMatrix numRows numCols [] 1

    putStrLn " "

    --putStrLn "Matriz de Entrada:"
    --mapM_ print m

    --putStrLn "\n"

    --mResposta <- startGame m [] numInt numRows numCols

    --putStrLn "Matriz de Saida:"
    --mapM_ print mResposta

    putStrLn " "