import Data.List
import Data.Maybe

data State = Started | CanStart | ForceWhite deriving Show
data Color = Black | White deriving (Eq, Show)

-- class Pretty capable of pretty printing
class Pretty a where
	pretty :: a -> String
	listPretty :: [a] -> String
	listListPretty :: [[a]] -> String

instance Pretty Color where
	pretty White = "."
	pretty Black = "#"
	listPretty (a:as) = pretty a ++ listPretty as
	listPretty [] = ""
	listListPretty (a:as) = listPretty a ++ "\n" ++ listListPretty as
	listListPretty [] = ""

instance (Pretty a) => Pretty (Maybe a) where
	pretty Nothing = "?"
	pretty (Just a) = pretty a
	listPretty (a:as) = pretty a ++ listPretty as
	listPretty [] = ""
	listListPretty (a:as) = listPretty a ++ "\n" ++ listListPretty as
	listListPretty [] = ""

-- flatten - flatten an list by one degree
flatten :: [[a]] -> [a]
flatten = foldl (++) []

-- fillLeftmost - given set of rules and known cells, return left-most possible filling
fillLeftmost :: [Int] -> [Maybe Color] -> Maybe [(Int, Color)]
fillLeftmost rules known = do
	x <- listToMaybe $ fillLeftmostFirst rules known
	return $ mark x

-- fillRigtmost - given set of rules and known cells, return right-most possible filling
fillRightmost :: [Int] -> [Maybe Color] -> Maybe [(Int, Color)]
fillRightmost rules known = do
	x <- listToMaybe $ fillLeftmostFirst (reverse rules) (reverse known)
	return $ mark $ reverse x

-- fillLeftmostFirst - given set of rules and kown cells, return all possible fillings (if any), left-most first
fillLeftmostFirst :: [Int] -> [Maybe Color] -> [[Color]]
fillLeftmostFirst = fillLeftmostFirstLoop CanStart

-- fillLeftmostFirstLoop - recursive function serving as "main" part of fillLeftmostFirst function
fillLeftmostFirstLoop :: State -> [Int] -> [Maybe Color] -> [[Color]]
fillLeftmostFirstLoop _ [] [] = [[]]
fillLeftmostFirstLoop _ (0:rulex) known = fillLeftmostFirstLoop ForceWhite rulex known
fillLeftmostFirstLoop _ (rule:rulex) [] = []

fillLeftmostFirstLoop Started (rule:rulex) (col:knownx) =
	if col == Just White
		then []
		else [Black : rest | rest <- fillLeftmostFirstLoop Started (rule-1:rulex) knownx]

fillLeftmostFirstLoop ForceWhite rules (col:knownx) =
	if col == Just Black
		then []
		else [White : rest | rest <- fillLeftmostFirstLoop CanStart rules knownx]

fillLeftmostFirstLoop CanStart [] (col:knownx) =
	if col == Just Black
		then []
		else [White : rest | rest <- fillLeftmostFirstLoop CanStart [] knownx]
fillLeftmostFirstLoop CanStart (rule:rulex) (col:knownx) =
	case col of
		Just Black -> [Black : rest | rest <- fillLeftmostFirstLoop Started (rule-1:rulex) (knownx)]
		Just White -> [White : rest | rest <- fillLeftmostFirstLoop CanStart (rule:rulex) (knownx)]
		Nothing ->
			[Black : rest | rest <- fillLeftmostFirstLoop Started (rule-1:rulex) (knownx)] ++
			[White : rest | rest <- fillLeftmostFirstLoop CanStart (rule:rulex) (knownx)]

-- mark - given filled row (col) add a number to each cell saying how many black blocks are before given cell
mark :: [Color] -> [(Int, Color)]
mark = markLoop White 0

-- makrLoop - recursive function servin as "main" part of mark fucntion
markLoop :: Color -> Int -> [Color] -> [(Int, Color)]
markLoop _ _ [] = []
markLoop lastCol lastNum (col:cols) =
	if lastCol == Black || col == White
		then (lastNum, col) : markLoop col lastNum cols
		else (lastNum+1, col) : markLoop col (lastNum+1) cols

-- fillDat - given set of rules and known cells, fill all possible cells
fillDat :: [Int] -> [Maybe Color] -> Maybe [Maybe Color]
fillDat rules dat = do
	l <- fillLeftmost rules dat
	r <- fillRightmost rules dat
	return $ zipWith checkKnown l r

-- checkKnown - given marked left-most and right-most filling, fill all known (overlapping) filled sells
checkKnown :: (Int, Color) -> (Int, Color) -> Maybe Color
checkKnown (xi, xc) (yi, yc) =
	if xi == yi && xc == yc
		then (Just xc)
		else Nothing

-- walkOneDirectionRules - resolve (once each) all row-rules
walkOneDirectionRules :: [[Int]] -> [[Maybe Color]] -> Maybe [[Maybe Color]]
walkOneDirectionRules [] [] = Just []
walkOneDirectionRules (rules:rulesx) (known:knownx) = do
	h <- fillDat rules known
	b <- walkOneDirectionRules rulesx knownx
	return (h:b)

-- walkAllRules - resolve (onec each) all rules
walkAllRules :: ([[Int]], [[Int]]) -> [[Maybe Color]] -> Maybe [[Maybe Color]]
walkAllRules (horizontal, vertical) known = do
	knownUpdated <- walkOneDirectionRules horizontal known
	knownFinal <- walkOneDirectionRules vertical (transpose knownUpdated)
	return $ transpose knownFinal

-- solve - givenrules, solve a riddle
solve :: ([[Int]], [[Int]]) -> [[Maybe Color]]
solve rules =
	case res of
		Nothing -> error "Insolvable riddle"
		Just x -> x
		where res = solveSub rules

-- solveSub - supplementary function for function solve
solveSub :: ([[Int]], [[Int]]) -> Maybe [[Maybe Color]]
solveSub (horizontal, vertical) = solveLoop (horizontal, vertical) empty
	where	
		empty = take (length horizontal) (cycle [singleEmpty])
		singleEmpty = take (length vertical) (cycle [Nothing])

-- solveLoop - recursive function, given set of all rules and known cells, solve a riddle
solveLoop :: ([[Int]], [[Int]]) -> [[Maybe Color]] -> Maybe [[Maybe Color]]
solveLoop rules knowns = do
	solved <- solveBasicLoop rules knowns
	if allKnown $ flatten solved
		then return solved
		else
			let
				guessedBlack = solveLoop rules $ guess Black knowns
				guessedWhite = solveLoop rules $ guess White knowns
			in
				case guessedBlack of
					Just x -> return x
					Nothing ->
						case guessedWhite of
							Just x -> return x
							Nothing -> fail "Insolvable riddle"

-- allKnown - seek if there is any unknown cell
allKnown :: [Maybe Color] -> Bool
allKnown [] = True
allKnown (Nothing:_) = False
allKnown (Just _:rest) = allKnown rest

-- guess - given color and set of known cells, fill first unknown cell with color
guess :: Color -> [[Maybe Color]] -> [[Maybe Color]]
guess col (row:table) =
	if elem Nothing row
		then guessInRow col row : table
		else row : guess col table

-- guessInRow - supplementary function for function guess
guessInRow :: Color -> [Maybe Color] -> [Maybe Color]
guessInRow col (Nothing:row) = (Just col) : row
guessInRow col (Just a:row) = (Just a) : guessInRow col row

-- solveBasicLoop - recursive function, given set of all rules and known cells, solve the riddle without guessing
solveBasicLoop :: ([[Int]], [[Int]]) -> [[Maybe Color]] -> Maybe [[Maybe Color]]
solveBasicLoop rules known = do
	new <- walkAllRules rules known
	if known == new
		then return known
		else solveBasicLoop rules new

-- loadInput - read initial input, return set of rules
loadInput :: IO ([[Int]], [[Int]])
loadInput = do
	height <- readInt
	width <- readInt
	rowRules <- sequence $ take height $ cycle [readInts]
	colRules <- sequence $ take width $ cycle [readInts]
	return (rowRules, colRules)

-- readInt - read int from input
readInt :: IO Int
readInt = do
	input <- getLine
	return (read input :: Int)

-- readInts - read list of ints from input
readInts :: IO [Int]
readInts = do
	input <- getLine
	return (map read $ words input :: [Int])

-- main - well, main function
main :: IO()
main = do
	input <- loadInput
	putStrLn $ listListPretty $ solve input
