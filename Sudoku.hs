module Sudoku where
import Test.QuickCheck
import Data.Char
import Data.List
import Data.Maybe
------------------------------------------------------------------------------
-- | Representation of sudoku puzzles (allows some junk)
type Cell = Maybe Int -- a single cell
type Row  = [Cell]    -- a row is a list of cells
data Sudoku = Sudoku [Row]
  deriving (Show, Eq)
rows :: Sudoku -> [Row]
rows (Sudoku ms) = ms
-- | A sample sudoku puzzle
example :: Sudoku
example =
    Sudoku
      [ [j 3,j 6,n  ,n  ,j 7,j 1,j 2,n  ,n  ]
      , [n  ,j 5,n  ,n  ,n  ,n  ,j 1,j 8,n  ]
      , [n  ,n  ,j 9,j 2,n  ,j 4,j 7,n  ,n  ]
      , [n  ,n  ,n  ,n  ,j 1,j 3,n  ,j 2,j 8]
      , [j 4,n  ,n  ,j 5,n  ,j 2,n  ,n  ,j 9]
      , [j 2,j 7,n  ,j 4,j 6,n  ,n  ,n  ,n  ]
      , [n  ,n  ,j 5,j 3,n  ,j 8,j 9,n  ,n  ]
      , [n  ,j 8,j 3,n  ,n  ,n  ,n  ,j 6,n  ]
      , [n  ,n  ,j 7,j 6,j 9,n  ,n  ,j 4,j 3]
      ]
  where
    n = Nothing
    j = Just
-- * A1
-- | allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku (replicate 9(replicate 9 Nothing))


-- * A2
-- | isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle
isSudoku :: Sudoku -> Bool
isSudoku (Sudoku ms) = length ms == 9 && all isValidRow ms
    where
      isValidRow :: Row -> Bool
      isValidRow row = length row == 9 && all isValidCell row

      isValidCell :: Cell -> Bool
      isValidCell (Just n) = n >= 1 && n <= 9
      isValidCell Nothing = True


-- * A3
-- | isFilled sud checks if sud is completely filled in,
-- i.e. there are no blanks
isFilled :: Sudoku -> Bool
isFilled (Sudoku ms) = all isValidRow ms
    where
      isValidRow :: Row -> Bool
      isValidRow row = all isValidCell row
      isValidCell :: Cell -> Bool
      isValidCell (Just _) = True
      isValidCell Nothing = False
------------------------------------------------------------------------------
-- * B1
-- | printSudoku sud prints a nice representation of the sudoku sud on
-- the screen
printSudoku :: Sudoku -> IO ()
printSudoku (Sudoku rows) = printRows rows
  where
    printRows :: [Row] -> IO()
    printRows [] = return ()
    printRows (r:rs) = do printRow r
                          printRows rs
    printRow :: Row -> IO()
    printRow [] = putStrLn " "
    printRow (cell:cells) = do printCell cell
                               printRow cells
    printCell :: Cell -> IO()
    printCell (Just digit) = putStr (show digit)
    printCell Nothing = putStr "."
-- * B2
-- | readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku filePath = do
    content <- readFile filePath
    maybe (error "Not a Sudoku") return (checkSudoku content)
checkSudoku :: String -> Maybe Sudoku
checkSudoku content = do
          rowsList <- validateLength 9 (lines content)
          rows <- checkRows rowsList
          return (Sudoku rows)
validateLength :: Int -> [a] -> Maybe [a]
validateLength expectedLength xs
    | length xs == expectedLength = Just xs
    | otherwise = Nothing
checkRows :: [String] -> Maybe [Row]
checkRows [] = Just []
checkRows (row:rest) = do
    checkedRow <- checkRow row
    checkedRest <- checkRows rest
    return (checkedRow : checkedRest)
checkRow :: String -> Maybe Row
checkRow [] = Just []
checkRow (cellChar:rest) = do
  checkedCell <- checkCell cellChar
  checkedRest <- checkRow rest
  return (checkedCell : checkedRest)
checkCell :: Char -> Maybe Cell
checkCell '.' = Just Nothing
checkCell ch
  | isDigit ch = Just(Just(digitToInt ch))
  | otherwise = Nothing
isThisDigit :: Char -> Bool
isThisDigit ch = ch >= '1' && ch <= '9'
------------------------------------------------------------------------------
-- * C1
-- | cell generates an arbitrary cell in a Sudoku
cell :: Gen (Cell)
cell = frequency [(9, return Nothing), (1, Just <$> choose (1,9))] -- <$> fmap listor, frequency: random, nothing, generator
-- * C2
-- | an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where -- Type class arbitrary
  arbitrary = do
    rowsList <- vectorOf 9 (vectorOf 9 (cell))
    return (Sudoku rowsList)
 -- hint: get to know the QuickCheck function vectorOf
-- * C3
prop_Sudoku :: Sudoku -> Bool
prop_Sudoku sudoku = isSudoku sudoku
  -- hint: this definition is simple!
------------------------------------------------------------------------------
type Block = [Cell] -- a Row is also a Cell
-- * D1
isOkayBlock :: Block -> Bool
isOkayBlock block = not (hasDuplicates (filter isValidCell block))
  where
    isValidCell :: Maybe Int -> Bool
    isValidCell (Just n) = n >= 1 && n <= 9
    isValidCell Nothing  = False

hasDuplicates :: Eq a => [a] -> Bool -- kollar duplicates
hasDuplicates []     = False
hasDuplicates (x:xs) = elem x xs || hasDuplicates xs  -- || = or operator för bools
-- * D2
blocks :: Sudoku -> [Block]
blocks (Sudoku rows) = [blockAt i j | i <- [0, 3, 6], j <- [0, 3, 6]]
  where
    blockAt :: Int -> Int -> Block
    blockAt rowStart colStart = [cellAt (rowStart + i)(colStart + j) | i <- [0..2], j <- [0..2]]
    cellAt :: Int -> Int -> Cell
    cellAt row col = (rows !! row) !! col
prop_blocks_lengths :: Sudoku -> Bool
prop_blocks_lengths sudoku = all checkBlockLength (blocks sudoku)
  where
    checkBlockLength :: Block -> Bool
    checkBlockLength block = length block == 9
-- * D3
isOkay :: Sudoku -> Bool
isOkay sudoku = all isOkayBlock (rows sudoku) && all isOkayBlock (transpose (rows sudoku))-- använda funktionen blocks i denna funktion
---- Part A ends here --------------------------------------------------------
------------------------------------------------------------------------------
---- Part B starts here ------------------------------------------------------
-- | Positions are pairs (row,column),
-- (0,0) is top left corner, (8,8) is bottom left corner
type Pos = (Int,Int)


-- * E1
blanks :: Sudoku -> [Pos]
blanks (Sudoku rows) = filter isNothing everyPos
   where everyPos = [(row, col) | row <- [0..8], col <- [0..8]]
         isNothing :: Pos -> Bool
         isNothing (row, col) = --f (rows !! row !! col)
          case rows !! row !! col of
             Just n -> False
             Nothing -> True
          --f :: Maybe Int -> Bool
          --f (Just n) = False
          --f Nothing = True
--data Maybe a = Just a | Nothing
--allBlankSudoku :: Sudoku
--allBlankSudoku = Sudoku (replicate 9(replicate 9 Nothing))

prop_blanks_allBlanks :: Bool
prop_blanks_allBlanks = length ( blanks allBlankSudoku ) == 9*9

-- * E2
(!!=) :: [a] -> (Int,a) -> [a]
[] !!= _         = error "out of bounds"
(x:xs) !!= (0,y) = (y:xs)
(x:xs) !!= (n,y) = (x : xs !!= (n-1, y))


-- | i < 0 || i > length (x:xs) - 1 = error "Out of bounds"
--                 | otherwise                      = (y:xs)

prop_bangBangEquals_correct :: [Int] -> Int -> Int -> Bool
prop_bangBangEquals_correct xs i y
    | i < 0 || i >= length xs = True
    | otherwise =
      updatedList !! i == y
    where
      updatedList = xs !!= (i,y)

-- * E3
update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update (Sudoku rows) (row, col) newDigit = Sudoku (updateRow rows row (updateCell (rows !! row) col newDigit))

updateRow :: [Row] -> Int -> Row -> [Row]
updateRow rows i newRow = take i rows ++ [newRow] ++ drop (i + 1) rows

updateCell :: Row -> Int -> Maybe Int -> Row
updateCell row col newDigit = take col row ++ [newDigit] ++ drop (col + 1) row

prop_update_updated :: Sudoku -> Pos -> Maybe Int -> Bool
prop_update_updated (Sudoku rows) (row, col) digit | (row < 0 || col < 0) || (row >= length rows || col >= length rows)   = error "out of bounds"
                                                   | otherwise                                                            =
                                                                                                                             case update (Sudoku rows) (row, col) digit of
                                                                                                                              Sudoku updatedRows -> updatedRows !! row !! col == digit
  
  --(update (Sudoku rows) (row, col) digit) !! row !! col == digit
------------------------------------------------------------------------------
-- * F1
solve :: Sudoku -> Maybe Sudoku
solve sudoku 
  | not (isSudoku sudoku) || not (isOkay sudoku) = Nothing -- om det inte är ok så gör not uttrycket till true
  | otherwise = case solve' sudoku (blanks sudoku) of -- blanks sudoku--> lista av tomma postitioner i sudoku grid
                  [] -> Nothing
                  (firstS:_) -> Just firstS
  where 
    solve' :: Sudoku -> [Pos] -> [Sudoku]
    solve' sudoku [] = [sudoku]
    solve' sudoku (xy:xys) = [s | value <- [1..9], 
                                let updatedSudoku = update sudoku xy (Just value),
                                  isOkay updatedSudoku,
                                  s <- solve' updatedSudoku xys]  

-- 
--xy:xys positioner

--solve :: Sudoku -> Maybe Sudoku
-- Kolla om det är en sudoku: !(isSudoku (Sudoku rows)) = error "not at sudoku"--dålig sudoku
--solve sudoku = case solve' sudoku (blanks sudoku) of
--                  [] -> Nothing
--                  (firstS:_) -> Just firstS 
      
--      where solve' :: Sudoku -> [Pos] -> [Sudoku]
--            solve' sudoku []       = [sudoku]
--            solve' sudoku (xy:xys) =  catMaybes $ do
--                                      value <- [1..9]
--                                      let updatedSudoku = update sudoku pos (Just value)
--                                      guard (isOkay updatedSudoku)
--                                      solve' updatedSudoku xys
--
              -- sätt in nu digit med update
              -- Kolla om sudoku fortfarande är valid med isOkay



-- * F2
readAndSolve :: FilePath -> IO()
readAndSolve filePath = do
        sudoku <- readSudoku filePath
        case solve sudoku of
          Nothing -> print "no solution"
          Just n -> printSudoku n
          



-- * F3
isSolutionOf :: Sudoku -> Sudoku -> Bool
isSolutionOf sudoku1 sudoku2 | (isOkay sudoku1) && (blanks sudoku1) == []    = digitPositions sudoku2 sudoku1
                             | otherwise                                     = True
                                     where digitPositions :: Sudoku -> Sudoku -> Bool
                                           digitPositions (Sudoku rows2) (Sudoku rows1) = all isSomething everyPos
                                            where everyPos = [(row, col) | row <- [0..8], col <- [0..8]]
                                                  isSomething :: Pos -> Bool
                                                  isSomething (row, col) = case rows2 !! row !! col of
                                                    Just n -> rows1 !! row !! col == Just n
                                                    Nothing -> True

                                                    
-- no blanks all blocks okay  (Är slltså en sulotion)
-- för varje pos jämför om det är samma digit (Alltså)


-- * F4
--prop_SolveSound :: Sudoku -> Property

-- GLÖM INTE F4!!!!!!

