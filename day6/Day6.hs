import AOC2024.Support.Lexer as Lexer
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Tuple as Tuple
import qualified Options.Applicative as Options
import qualified Options.Applicative.Simple as SimpleOptions
import System.IO
import qualified Text.ParserCombinators.ReadP as ReadP
import qualified Text.ParserCombinators.ReadPrec as ReadPrec
import Text.Read (readListPrec, readListPrecDefault, readPrec)

data WorldElement = FreeSpace | Obstacle | VisitedSpace | Guard

newtype WorldElementRow = Row [WorldElement]

type World = Map.Map (Int, Int) WorldElement

data TraversalDirection = MoveUp | MoveLeft | MoveDown | MoveRight

move :: (Int, Int) -> TraversalDirection -> (Int, Int)
turn :: TraversalDirection -> TraversalDirection
worldRowAsList :: WorldElementRow -> [WorldElement]
parseWorld :: [Lexer.Token] -> World
part1 :: World -> Int
part2 :: World -> Int
main :: IO ()
main =
  do
    (filepath, ()) <- programOptions
    handle <- openFile filepath ReadMode
    tokens <- lexFile (Just "|,") handle
    print $ part1 $ parseWorld tokens
    print $ part2 $ parseWorld tokens
    return ()

part1 world =
  let visitedWorld = executeGuardsPathUntilExit world
   in length $ Map.filter isVisited visitedWorld

part2 world =
  let guardPosition = findGuardLocation world
      isVisitedButNotStart location value =
        if location == guardPosition
          then
            False
          else case (location, value) of
            (_, VisitedSpace) -> True
            _ -> False
      visitedWorld = executeGuardsPathUntilExit world
      visitedSpaces = Map.keys $ Map.filterWithKey isVisitedButNotStart visitedWorld
   in length . (filter (\x -> x)) . (map doesGuardLoopInWorld) . (map (\x -> Map.insert x Obstacle world)) $ visitedSpaces

move (lineNo, colNo) MoveUp = (lineNo - 1, colNo)
move (lineNo, colNo) MoveLeft = (lineNo, colNo - 1)
move (lineNo, colNo) MoveDown = (lineNo + 1, colNo)
move (lineNo, colNo) MoveRight = (lineNo, colNo + 1)

turn MoveUp = MoveRight
turn MoveRight = MoveDown
turn MoveDown = MoveLeft
turn MoveLeft = MoveUp

isGuard :: WorldElement -> Bool
isGuard Guard = True
isGuard _ = False

isVisited :: WorldElement -> Bool
isVisited VisitedSpace = True
isVisited _ = False

findGuardLocation :: World -> (Int, Int)
findGuardLocation = Tuple.fst . head . (filter (\(_, b) -> isGuard b)) . Map.toList

executeGuardsPathUntilExitWithLocation :: ((Int, Int), TraversalDirection, World) -> World
executeGuardsPathUntilExit :: World -> World
executeGuardsPathUntilExit world =
  let guardLocation = findGuardLocation world
   in executeGuardsPathUntilExitWithLocation (guardLocation, MoveUp, world)
executeGuardsPathUntilExitWithLocation (guardLocation, direction, world) =
  let nextPosition = move guardLocation direction
      newlyVisitedWorld = Map.insert guardLocation VisitedSpace world
   in case Map.lookup nextPosition world of
        Just Obstacle ->
          executeGuardsPathUntilExitWithLocation (guardLocation, turn direction, newlyVisitedWorld)
        Just _ ->
          executeGuardsPathUntilExitWithLocation (nextPosition, direction, newlyVisitedWorld)
        Nothing ->
          newlyVisitedWorld

doesGuardLoopInWorldStep :: Set.Set ((Int, Int), TraversalDirection) -> (Int, Int) -> TraversalDirection -> World -> Bool
doesGuardLoopInWorld :: World -> Bool
doesGuardLoopInWorld world =
  let guardLocation = findGuardLocation world
   in doesGuardLoopInWorldStep Set.empty guardLocation MoveUp world
doesGuardLoopInWorldStep visitedSet guardLocation direction world =
  let nextPosition = move guardLocation direction
      nextVisitedSet = Set.insert (guardLocation, direction) visitedSet
      newlyVisitedWorld = Map.insert guardLocation VisitedSpace world
   in case (guardLocation, direction) `Set.member` visitedSet of
        True ->
          True -- Already visited this position going in the same direction. Found loop.
        False ->
          case Map.lookup nextPosition world of
            Just Obstacle ->
              doesGuardLoopInWorldStep nextVisitedSet guardLocation (turn direction) newlyVisitedWorld
            Just _ ->
              doesGuardLoopInWorldStep nextVisitedSet nextPosition direction newlyVisitedWorld
            Nothing ->
              False -- No loop found.

parseWorldStep :: [((Int, Int), WorldElement)] -> [Lexer.Token] -> Int -> World
parseWorldStep partialMap [] _ = Map.fromList partialMap
parseWorldStep partialMap (token : remaining) lineNo =
  case token of
    Lexer.Word line ->
      let strippedRemaining = dropWhile (not . isWord) remaining
          newRow = List.map Tuple.swap $ zip (worldRowAsList $ read line) (zip (repeat lineNo) (iterate increment 1))
       in parseWorldStep (partialMap ++ newRow) strippedRemaining (lineNo + 1)
    _ ->
      parseWorldStep partialMap remaining lineNo

parseWorld tokens = parseWorldStep [] tokens 1

toWorldElement :: Char -> ReadP.ReadP WorldElement
toWorldElement c =
  case c of
    '#' -> return Obstacle
    '.' -> return FreeSpace
    '^' -> return Guard
    _ -> fail "Invalid character found when reading WorldElement."

instance Read WorldElementRow where
  readPrec =
    ReadPrec.lift $ fmap (\x -> Row x) $ ReadP.many $ ReadP.get >>= toWorldElement
  readListPrec =
    readListPrecDefault

directionAsInt :: TraversalDirection -> Int
directionAsInt MoveUp = 0
directionAsInt MoveLeft = 1
directionAsInt MoveRight = 2
directionAsInt MoveDown = 3

instance Eq TraversalDirection where
  (==) a b = (directionAsInt a) == (directionAsInt b)

instance Ord TraversalDirection where
  (<=) a b = (directionAsInt a) <= (directionAsInt b)

instance Show WorldElement where
  showsPrec _ Obstacle = showString "#"
  showsPrec _ FreeSpace = showString "."
  showsPrec _ Guard = showString "^"
  showsPrec _ VisitedSpace = showString "X"

worldRowAsList (Row l) = l

isWord :: Lexer.Token -> Bool
isWord (Lexer.Word _) = True
isWord _ = False

increment :: Int -> Int
increment a = a + 1

programOptions :: IO (String, ())
programOptions =
  SimpleOptions.simpleOptions
    "0.1"
    "AOC 2024 Day 6 Solver"
    "Solves the day 6 problem for Advent of Code 2024."
    (Options.strArgument $ Options.metavar "FILEPATH")
    SimpleOptions.empty