module Lib where

--startGame :: Grid -> Grid -> Int -> Int -> Int -> Int -> IO ()
--startGame m mResp n numRows numCols currentIter = do
--    if (currentIter <= n)
--        then do
--            putStrLn $ "Iteração " ++ show currentIter ++ ":"
--            mResp <- verificaMatriz m [] (Coord 0 0) numRows numCols
--            mapM_ print mResp
--            putStrLn " "
--
--            -- Checa estabilidade
--            if mResp == m
--                then do
--                    putStrLn "System estabilizado."
--                    putStrLn $ "Número de passos até estabilizar: " ++ show currentIter
--                else
--                    startGame mResp mResp n numRows numCols (currentIter + 1)
--        else return ()