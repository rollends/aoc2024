import AOC2024.Support.Lexer as Lexer
import qualified Data.List as List
import qualified Data.Map as Map
import GHC.Utils.Panic (sorry)
import Options.Applicative
import Options.Applicative.Simple
import System.IO

data LetterTag = Unexplored | Explored [Int]

data ExplorationState = ExpectX | ExpectM Int | ExpectA Int | ExpectS Int

type LetterGrid = Map.Map (Int, Int) Char

type TaggedLetterGrid = Map.Map (Int, Int) (Char, LetterTag)

parseGrid :: [Lexer.Token] -> LetterGrid
countXMAS :: LetterGrid -> Int
exploreGridForXMAS :: TaggedLetterGrid -> (Int, Int) -> ExplorationState -> TaggedLetterGrid
countCrossMAS :: LetterGrid -> Int

makeTaggedLetterGrid :: LetterGrid -> TaggedLetterGrid
main :: IO ()
main =
  do
    (filepath, ()) <- programOptions
    handle <- openFile filepath ReadMode
    tokens <- lexFile Nothing handle
    print $ countXMAS $ parseGrid tokens
    print $ countCrossMAS $ parseGrid tokens
    return ()

directions :: [(Int, Int)]
directions = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]

directionIndices :: [Int]
directionIndices = (map snd) . (zip $ directions) $ iterate increment 0

indexForDirection :: (Int, Int) -> Int
indexForDirection direction =
  case List.elemIndex direction directions of
    Just i -> i
    Nothing -> sorry "Unexpected direction provided."

neighbours :: (Int, Int) -> [(Int, Int)]
neighbours (lineNo, colNo) = map (\(a, b) -> (a + lineNo, b + colNo)) directions

getMatchCountAtIndexWithExpectation :: TaggedLetterGrid -> (Int, Int) -> ExplorationState -> Int
getMatchCountAtIndexWithExpectation grid index (ExpectM direction) =
  case Map.lookup index grid of
    Just ('M', Explored counts) -> counts !! direction
    Just ('M', Unexplored) -> sorry "Expected vertex to be explored, but it wasn't."
    _ -> 0
getMatchCountAtIndexWithExpectation grid index (ExpectA direction) =
  case Map.lookup index grid of
    Just ('A', Explored counts) -> counts !! direction
    Just ('A', Unexplored) -> sorry "Expected vertex to be explored, but it wasn't."
    _ -> 0
getMatchCountAtIndexWithExpectation grid index (ExpectS direction) =
  case Map.lookup index grid of
    Just ('S', Explored counts) -> counts !! direction
    Just ('S', Unexplored) -> sorry "Expected vertex to be explored, but it wasn't."
    _ -> 0
getMatchCountAtIndexWithExpectation grid index ExpectX =
  case Map.lookup index grid of
    Just ('X', Explored counts) -> sum counts
    Just ('X', Unexplored) -> sorry "Expected vertex to be explored, but it wasn't."
    _ -> 0

exploreGridForXMAS grid (lineNo, colNo) ExpectX =
  case Map.lookup (lineNo, colNo) grid of
    Just ('X', Unexplored) ->
      let exploreNextCharacter foldedGrid (direction, neighbourIndex) = exploreGridForXMAS foldedGrid neighbourIndex (ExpectM direction)
          exploredGrid = foldl exploreNextCharacter grid $ zip directionIndices $ neighbours (lineNo, colNo)
          matchesFound = map (\(dir, i) -> getMatchCountAtIndexWithExpectation exploredGrid i (ExpectM dir)) $ zip directionIndices $ neighbours (lineNo, colNo)
          updatedGrid = Map.insert (lineNo, colNo) ('X', Explored matchesFound) exploredGrid
       in exploreGridForXMAS updatedGrid (lineNo, colNo + 1) ExpectX
    Just ('M', Unexplored) ->
      exploreGridForXMAS (exploreGridForXMAS grid (lineNo, colNo) (ExpectM 0)) (lineNo, colNo + 1) ExpectX
    Just ('A', Unexplored) ->
      exploreGridForXMAS (exploreGridForXMAS grid (lineNo, colNo) (ExpectA 0)) (lineNo, colNo + 1) ExpectX
    Just ('S', Unexplored) ->
      exploreGridForXMAS (exploreGridForXMAS grid (lineNo, colNo) (ExpectS 0)) (lineNo, colNo + 1) ExpectX
    Just _ ->
      exploreGridForXMAS grid (lineNo, colNo + 1) ExpectX
    Nothing ->
      case Map.lookup (lineNo + 1, 1) grid of
        Just _ -> exploreGridForXMAS grid (lineNo + 1, 1) ExpectX
        Nothing -> grid
exploreGridForXMAS grid (lineNo, colNo) (ExpectM _) =
  case Map.lookup (lineNo, colNo) grid of
    Just ('M', Unexplored) ->
      let exploreNextCharacter foldedGrid (direction, neighbourIndex) = exploreGridForXMAS foldedGrid neighbourIndex (ExpectA direction)
          exploredGrid = foldl exploreNextCharacter grid $ zip directionIndices $ neighbours (lineNo, colNo)
          matchesFound = map (\(dir, i) -> getMatchCountAtIndexWithExpectation exploredGrid i (ExpectA dir)) $ zip directionIndices $ neighbours (lineNo, colNo)
          updatedGrid = Map.insert (lineNo, colNo) ('M', Explored matchesFound) exploredGrid
       in updatedGrid
    _ -> grid
exploreGridForXMAS grid (lineNo, colNo) (ExpectA _) =
  case Map.lookup (lineNo, colNo) grid of
    Just ('A', Unexplored) ->
      let exploreNextCharacter foldedGrid (direction, neighbourIndex) = exploreGridForXMAS foldedGrid neighbourIndex (ExpectS direction)
          exploredGrid = foldl exploreNextCharacter grid $ zip directionIndices $ neighbours (lineNo, colNo)
          matchesFound = map (\(dir, i) -> getMatchCountAtIndexWithExpectation exploredGrid i (ExpectS dir)) $ zip directionIndices $ neighbours (lineNo, colNo)
          updatedGrid = Map.insert (lineNo, colNo) ('A', Explored matchesFound) exploredGrid
       in updatedGrid
    _ -> grid
exploreGridForXMAS grid (lineNo, colNo) (ExpectS _) =
  case Map.lookup (lineNo, colNo) grid of
    Just ('S', Unexplored) -> Map.insert (lineNo, colNo) ('S', Explored $ replicate (length directions) 1) grid
    _ -> grid

countXMAS grid =
  let exploredGrid = exploreGridForXMAS (makeTaggedLetterGrid grid) (1, 1) ExpectX
   in Map.foldl
        ( \count v ->
            case v of
              ('X', Explored counts) -> count + sum counts
              _ -> count
        )
        0
        exploredGrid

isCrossMASHere :: TaggedLetterGrid -> (Int, Int) -> (Char, LetterTag) -> Bool
isCrossMASHere grid (lineNo, colNo) v =
  case v of
    ('A', Explored _) ->
      let topLeft = getMatchCountAtIndexWithExpectation grid (lineNo - 1, colNo - 1) $ ExpectM $ indexForDirection (1, 1)
          topRight = getMatchCountAtIndexWithExpectation grid (lineNo - 1, colNo + 1) $ ExpectM $ indexForDirection (1, -1)
          botLeft = getMatchCountAtIndexWithExpectation grid (lineNo + 1, colNo - 1) $ ExpectM $ indexForDirection (-1, 1)
          botRight = getMatchCountAtIndexWithExpectation grid (lineNo + 1, colNo + 1) $ ExpectM $ indexForDirection (-1, -1)
       in (topLeft + topRight + botLeft + botRight) >= 2
    _ -> False

countCrossMAS grid =
  let exploredGrid = exploreGridForXMAS (makeTaggedLetterGrid grid) (1, 1) ExpectX
   in Map.size $ Map.filter (\v -> v) $ Map.mapWithKey (isCrossMASHere exploredGrid) exploredGrid

parseGridIntoExistingGrid :: [Lexer.Token] -> LetterGrid -> Int -> LetterGrid

parseGrid tokens = parseGridIntoExistingGrid tokens Map.empty 1

increment :: Int -> Int
increment a = a + 1

parseGridIntoExistingGrid [] grid _ = grid
parseGridIntoExistingGrid (Lexer.Newline : rest) grid lineNumber = parseGridIntoExistingGrid rest grid (lineNumber + 1)
parseGridIntoExistingGrid (Lexer.Delimiter : rest) grid lineNumber = parseGridIntoExistingGrid rest grid lineNumber
parseGridIntoExistingGrid ((Lexer.Whitespace _) : rest) grid lineNumber = parseGridIntoExistingGrid rest grid lineNumber
parseGridIntoExistingGrid ((Lexer.Word string) : rest) grid lineNumber =
  let characterWithIndex = zip3 string (repeat lineNumber) (iterate increment 1)
      partialMapAsList = map (\(char, lineNo, colNo) -> ((lineNo, colNo), char)) characterWithIndex
   in parseGridIntoExistingGrid rest (Map.union (Map.fromList partialMapAsList) grid) lineNumber

makeTaggedLetterGrid = Map.map (\char -> (char, Unexplored))

programOptions :: IO (String, ())
programOptions =
  simpleOptions
    "0.1"
    "AOC 2024 Day 4 Solver"
    "Solves the day 4 problem for Advent of Code 2024."
    (strArgument $ metavar "FILEPATH")
    Options.Applicative.Simple.empty

instance Show LetterTag where
  showsPrec _ Unexplored = showString "U"
  showsPrec _ (Explored k) = showString $ "E " ++ (show k)
