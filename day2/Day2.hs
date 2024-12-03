import AOC2024.Support.Lexer as Lexer
import Options.Applicative
import Options.Applicative.Simple
import System.IO

data Report = Levels [Int]

data SafetyFlag = Safe | Unsafe

isSafe :: SafetyFlag -> Bool
isSafe Safe = True
isSafe _ = False 

computeSafety :: Report -> SafetyFlag
computeSafetyWithSubstitute :: Report -> SafetyFlag
part1 :: [Report] -> Int
part2 :: [Report] -> Int

buildReports :: (MonadFail m) => [Lexer.Token] -> [Report] -> m [Report]
programOptions :: IO (String, ())
programOptions =
  simpleOptions
    "0.1"
    "AOC 2024 Day 1 Solver"
    "Solves the day 1 problem for Advent of Code 2024."
    (strArgument $ metavar "FILEPATH")
    empty

main :: IO ()
main =
  do
    (filepath, ()) <- programOptions
    handle <- openFile filepath ReadMode
    tokens <- lexFile Nothing handle
    reports <- buildReports tokens []
    print $ part1 reports
    print $ part2 reports
    return ()

part1 reports = length $ filter isSafe $ map computeSafety reports 
part2 reports = length $ filter isSafe $ map computeSafetyWithSubstitute reports 


computeSafetyFixViolationN2 :: [Int] -> [Int] -> SafetyFlag
computeSafetyFixViolationN2 _ [] = Unsafe
computeSafetyFixViolationN2 start (middle:end) =
  case computeSafety $ Levels $ start ++ end of
    Safe -> Safe
    Unsafe -> computeSafetyFixViolationN2 (start ++ [middle]) end

computeSafetyFixViolation :: Report -> SafetyFlag
computeSafetyFixViolation (Levels list) =
  computeSafetyFixViolationN2 [] list


computeSafetyWithSubstitute report =
  case computeSafety report of 
    Safe -> Safe
    Unsafe -> computeSafetyFixViolation report

isIncreasing :: Ord a => [a] -> Bool
isIncreasing [] = True
isIncreasing [_] = True
isIncreasing (x1:rest) =
  let 
    x2 = head rest 
  in
    (x2 > x1) && isIncreasing rest
isDecreasing :: Ord a => [a] -> Bool
isDecreasing [] = True
isDecreasing [_] = True
isDecreasing (x1:rest) =
  let 
    x2 = head rest 
  in
    (x2 < x1) && isDecreasing rest

computeSafetyIncreasing :: (Ord a, Num a) => [a] -> SafetyFlag
computeSafetyIncreasing [] = Safe
computeSafetyIncreasing [_] = Safe
computeSafetyIncreasing (x1:rest) =
  let 
    x2 = head rest
    d = x2 - x1
  in
    case (d >= 1) && (d <= 3) of
      False -> Unsafe
      True -> computeSafetyIncreasing rest

computeSafety (Levels list) =
  case (isIncreasing list, isDecreasing list) of
    (True, _) -> computeSafetyIncreasing list
    (_, True) -> computeSafetyIncreasing $ map (\x -> -x) list
    _ -> Unsafe

isNewline :: Token -> Bool
scanLevels :: [Token] -> [Int] -> [Int]

scanLevels [] levels = levels
scanLevels (a : r) levels =
  case a of
    Word s -> scanLevels r (levels ++ [read s])
    _ -> scanLevels r levels

buildReports [] reports = return reports
buildReports tokens reports =
  let (line, remainder) = break isNewline tokens
   in buildReports (tail remainder) (reports ++ [Levels $ scanLevels line []])

isNewline Lexer.Newline = True
isNewline _ = False

instance Show Report where
  showsPrec d (Levels a) = showsPrec d a