import AOC2024.Support.Lexer as Lexer
import qualified Data.List as List
import qualified Data.Map as Map
import Options.Applicative
import Options.Applicative.Simple
import System.IO

buildLists :: (MonadFail m) => [Lexer.Token] -> [Int] -> [Int] -> m ([Int], [Int])
part1 :: ([Int], [Int]) -> Int
part2 :: ([Int], [Int]) -> Int
part2_makeCountMap :: [Int] -> Map.Map Int (Int, Int) -> Map.Map Int (Int, Int)
part2_updateCounts :: Map.Map Int (Int, Int) -> [Int] -> Map.Map Int (Int, Int)
part2_computeSimilarityScore :: Map.Map Int (Int, Int) -> Int
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
    (list1, list2) <- buildLists tokens [] []
    print $ part1 (list1, list2)
    print $ part2 (list1, list2)
    return ()

part1 (listA, listB) = sum $ map (\(a, b) -> abs (a - b)) $ zip (List.sort listA) (List.sort listB)

part2 (listA, listB) =
  part2_computeSimilarityScore $ part2_updateCounts (part2_makeCountMap listA Map.empty) listB

part2_makeCountMap [] m = m
part2_makeCountMap (h : t) m =
  part2_makeCountMap t $ Map.insertWith (\(b, _) (a, _) -> (a + b, 0)) h (1, 0) m

part2_updateCounts counts [] = counts
part2_updateCounts counts (h : t) =
  part2_updateCounts (Map.adjust (\(v, a) -> (v, a + 1)) h counts) t

part2_computeSimilarityScore m =
  sum $ map (\(v, (a, b)) -> v * a * b) $ Map.toList m

isNewline :: Token -> Bool

buildLists [] la lb = return (la, lb)
buildLists tokens la lb =
  let (line, remainder) = break isNewline tokens
   in case line of
        [Lexer.Word a, Lexer.Whitespace _, Lexer.Word b] -> buildLists (tail remainder) (la ++ [read a]) (lb ++ [read b])
        _ -> fail "I was not able to parse a line in the file."

isNewline Lexer.Newline = True
isNewline _ = False