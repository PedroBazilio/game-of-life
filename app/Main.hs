{-# OPTIONS_GHC -Wno-missing-export-lists #-}
import Lib

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