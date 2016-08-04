module Main where
--module Sudoku where

import Test.QuickCheck
import System.IO
import Data.Char
import Data.List
import Data.Maybe
import System.Environment

-------------------------------------------------------------------------

data Sudoku = Sudoku [[Maybe Int]]
 deriving ( Show, Eq )

rows :: Sudoku -> [[Maybe Int]]
rows (Sudoku rs) = rs

cells :: Sudoku -> [Maybe Int]
cells (Sudoku sud) = foldr (++) [] sud

-- allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku =  Sudoku [ [ Nothing | x <- [1..9]] | y <- [1..9] ]

              
-- isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle
isSudoku :: Sudoku -> Bool
isSudoku (Sudoku sud) = is9Rows && allRowsIs9 && allValid
  where is9Rows    = length sud == 9
        allRowsIs9 = and [ length row == 9 | row <- sud ]
        allValid   = and [ isValidCell cell | cell <- concat sud ]

isValidCell :: Maybe Int -> Bool
isValidCell Nothing  = True
isValidCell (Just x) = x >= 1 && x <= 9

-- isSolved sud checks if sud is already solved, i.e. there are no blanks
isSolved :: Sudoku -> Bool
isSolved sud = Nothing `notElem` cells sud

-------------------------------------------------------------------------

-- printSudoku sud prints a representation of the sudoku sud on the screen
printSudoku :: Sudoku -> IO ()
printSudoku (Sudoku sud) = do 
  putStr $ unlines [ [ cellToChar cell | cell <- row ] | row <- sud ]

cellToChar :: Maybe Int -> Char
cellToChar Nothing  = '.'
cellToChar (Just x) = intToDigit x

-- readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku f = do  
  str  <- readFile f
  let sud = (Sudoku ( [ [ charToCell cell | 
            cell <- row ] | 
            row <- (lines str)] ))
  if isSudoku sud
    then return sud
    else do 
      error "Not a valid Sudoku"

-- Convert Char to a Sudoku cell
charToCell :: Char -> Maybe Int
charToCell '.'  = Nothing
charToCell cell = (Just (digitToInt cell))
       

-------------------------------------------------------------------------

-- cell generates an arbitrary cell in a Sudoku
cell :: Gen (Maybe Int)
cell = frequency
  [(9,return Nothing),
   (1, do c <- choose(1,9)
          return (Just c))]

-- an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- sequence [ sequence [ cell | j <- [1..9] ] | i <- [1..9] ]
       return (Sudoku rows)

-- Checks if it generates valid Sudoku tables.
-- Can be set to prop_Sudoku :: Sudoku -> Property to collect result
-- using function: prop_Sudoku (Sudoku sud) = collect sud (isSudoku (Sudoku sud))
prop_Sudoku :: Sudoku -> Bool
prop_Sudoku (Sudoku sud) = isSudoku (Sudoku sud)


-------------------------------------------------------------------------
type Block = [Maybe Int]

isOkayBlock :: Block -> Bool
isOkayBlock b = (catMaybes b) == (nub (catMaybes b))  

-- Extrude all possible blocks
blocks :: Sudoku -> [Block]
blocks (Sudoku sud) = sud ++ transpose sud ++ allSquares
  where allSquares  = [ concat 
                      [ get (a+b+c+d) 
                      | b <- [0,9,18], a <- [1,2,3]  ] 
                      | d <- [0,27,54], c <- [0,3,6] ]
        cellList = cells (Sudoku sud)
        get a = take 1 $ drop (a-1) cellList

prop_Blocks :: Sudoku -> Bool
prop_Blocks sud = length (blocks sud) == (3*9)
                  && (and [length block == 9 | block <- blocks sud])

-------------------------------------------------------------------------
-- E

-- Creating new data types for positions and cells
-- We did this for better testing
data Pos = Pos (Int,Int)
  deriving (Show, Eq)

instance Arbitrary Pos where
  arbitrary = do n <- choose (0,8)
                 m <- choose (0,8)
                 return (Pos (n,m))
                  

data Cell = Cell (Maybe Int)
  deriving (Show, Eq)

instance Arbitrary Cell where
  arbitrary = oneof [ return (Cell (Nothing))
                    , do n <- choose (1,9)
                         return (Cell (Just n))
                    ]

-- Return a coordinate of a blank cell
-- Gives exception if Sudoku is finished
blank :: Sudoku -> Pos
blank (Sudoku sud) = head [ Pos (x `div` 9, x `mod` 9) 
                          | x <- [0..80]
                          , cellList !! x == Nothing ]
  where cellList = concat sud

prop_Blank :: Sudoku -> Bool
prop_Blank sud = emptyCell == Nothing
  where (Pos(row, cell)) = blank sud
        emptyCell = head $ drop (row*8+cell) $ cells sud

-- Change value on a position in a list
(!!=) :: [a] -> (Int,a) -> [a]
(!!=) xs (pos, val) = take pos xs ++ [val] ++ drop (pos+1) xs

prop_Operator :: (Eq a) => [a] -> (Int, a) -> Property
prop_Operator xs (pos, val) = length xs >= pos && pos >= 0 ==> (ys !! pos) == val
  where ys = xs !!= (pos, val)

-- Change value on a position in a Sudoku
update :: Sudoku -> Pos -> Cell -> Sudoku
update (Sudoku sud) (Pos(row, cell)) (Cell val) = (Sudoku updated)
  where updated = take row sud ++ 
                  [((concat $ take 1 $ drop row sud) !!= (cell, val))] ++ 
                  drop (row+1) sud

-- Prop that checks if the position is really updated with the correct value
prop_Update :: Sudoku -> Pos -> Cell -> Property
prop_Update sud (Pos(row, cell)) (Cell val) = 
  -- Doing a double check that it is fair value
  isValidCell val && row >= 0 && row < 9 && cell >= 0 && cell < 9 
  ==> val == updated
  where updated = head 
                $ drop (row*9+cell) 
                $ cells 
                $ update sud (Pos(row, cell)) (Cell val)

-------------------------------------------------------------------------
-- F

isOkaySudoku sud = and [ isOkayBlock b | b <- (blocks sud)]

-- Solve with brute force
solve :: Sudoku -> Maybe Sudoku
solve sud
  | not (isOkaySudoku sud) = Nothing
  | isSolved sud           = Just sud
  | otherwise              = listToMaybe $ catMaybes $ map solve newSuds
  where newSuds            = [update sud (blank sud) (Cell (Just a)) | a <- [1..9]]
-- We could check that the cell is a valid move, but we don't care about
-- performance right now. It's not essential right now. 


-- Instructions that read and solve a Sudoku from a FilePath
readAndSolve :: FilePath -> IO ()
readAndSolve f = do
  sud1 <- readSudoku f
  let sud2 = solve sud1
  if sud2 == Nothing 
    then putStrLn "(no solution)" 
    else printSudoku (fromJust sud2)
  return ()


-- Takes two Sudokus and compares solutions
isSolutionOf :: Sudoku -> Sudoku -> Bool
isSolutionOf a b = if solved == Nothing
                     then False
                     else fromJust solved == a
  where solved = solve b


-- QuickCheck property for isSolutionOf
prop_SolveSound :: Sudoku -> Property
prop_SolveSound sud = isSudoku sud && isOkaySudoku sud 
                    ==> isSolutionOf (fromJust (solve sud)) sud
-- Currently finishes a lot of tests but get stuck on hard Sudoku
-- Can we submit again when we finished extra assignments to optimize? 


-------------------------------------------------------------------------
-- An example sudoku grid
example :: Sudoku
example =
  Sudoku
    [ [Just 3, Just 6, Nothing,  Nothing,Just 7, Just 1,   Just 2, Nothing,Nothing]
    , [Nothing,Just 5, Nothing,  Nothing,Nothing,Nothing,  Just 1, Just 8, Nothing]
    , [Nothing,Nothing,Just 9,   Just 2, Nothing,Just 4,   Just 7, Nothing,Nothing]

    , [Nothing,Nothing,Nothing,  Nothing,Just 1, Just 3,   Nothing,Just 2, Just 8]
    , [Just 4, Nothing,Nothing,  Just 5, Nothing,Just 2,   Nothing,Nothing,Just 9]
    , [Just 2, Just 7, Nothing,  Just 4, Just 6, Nothing,  Nothing,Nothing,Nothing]
    
    , [Nothing,Nothing,Just 5,   Just 3, Nothing,Just 8,   Just 9, Nothing,Nothing]
    , [Nothing,Just 8, Just 3,   Nothing,Nothing,Nothing,  Nothing,Just 6, Nothing]
    , [Nothing,Nothing,Just 7,   Just 6, Just 9, Nothing,  Nothing,Just 4, Just 3]
    ]

main = do  
    (file:_) <- getArgs  
    readAndSolve file