import AOC2024.Support.Lexer as Lexer
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe
import Data.Map.Internal (merge, preserveMissing, preserveMissing, zipWithMatched)
import qualified Data.Set as Set
import GHC.Utils.Panic (sorry)
import Options.Applicative
import Options.Applicative.Simple
import System.IO

data Rule = OrderViolation Int Int
data PrintRequest = Pages [Int]

parseProblem :: Monad m => [Lexer.Token] -> m ([Rule], [PrintRequest])
isValidRequest :: Map.Map Int [Rule] -> PrintRequest -> Bool
part1 :: ([Rule], [PrintRequest]) -> Int

correctRequestStep :: Map.Map Int [Rule] -> [Int] -> [Int] -> PrintRequest
correctRequest :: Map.Map Int [Rule] -> PrintRequest -> PrintRequest
part2 :: ([Rule], [PrintRequest]) -> Int

main :: IO ()
main =
  do
    (filepath, ()) <- programOptions
    handle <- openFile filepath ReadMode
    tokens <- lexFile (Just "|,") handle
    (rules, requests) <- parseProblem tokens
    print $ part1 (rules, requests)
    print $ part2 (rules, requests)
    return ()

part2 (rules, requests) =
  let
    ruleMap = makeRuleMap rules
    reverseRuleMap = makeReverseRuleMap rules
    badRequests = filter (not . isValidRequest ruleMap) requests
  in
    sum $ map (getMiddlePage . correctRequest reverseRuleMap) badRequests

part1 (rules, requests) =
  sum $ map getMiddlePage $ filter (isValidRequest $ makeRuleMap rules) requests


correctRequest ruleMap (Pages pgs) =
  correctRequestStep ruleMap [] pgs

correctRequestStep _ goodPages [] = Pages goodPages
correctRequestStep ruleMap goodPages (currentPage:remainingPages) =
  let
    applicableRules = Map.lookup currentPage ruleMap
    searchRemainingPagesFor (OrderViolation a _) = maybeToList $ List.elemIndex a remainingPages
    violationIndicesOf = List.sort . (List.concatMap searchRemainingPagesFor)
  in
    case applicableRules of
      Just rules ->
        case violationIndicesOf rules of
          (violationIndex:_) -> 
            let
              (before, after) = List.splitAt (violationIndex + 1) remainingPages
            in 
              correctRequestStep ruleMap goodPages (before ++ [currentPage] ++ after)
          [] -> 
            correctRequestStep ruleMap (goodPages ++ [currentPage]) remainingPages
      Nothing -> correctRequestStep ruleMap (goodPages ++ [currentPage]) remainingPages

getMiddlePage :: PrintRequest -> Int
getMiddlePage (Pages list) =
  let 
    midIndex = div (length list) 2
  in
    list !! midIndex

doOrderRulesPass :: [Rule] -> Int -> Set.Set Int -> Bool
doOrderRulesPass [] _ _ = True
doOrderRulesPass ((OrderViolation pgA pgB):rules) currentPage pagesSeen = 
  case pgA == currentPage of
    True ->
      if pgB `Set.member` pagesSeen then
        False
      else 
        doOrderRulesPass rules currentPage pagesSeen
    False -> doOrderRulesPass rules currentPage pagesSeen
  
isValidRequestOperator :: Map.Map Int [Rule] -> [Int] -> Set.Set Int -> Bool
isValidRequestOperator _ [] _ = True
isValidRequestOperator ruleMap (currentPage:pages) pagesSeen =
  case Map.lookup currentPage ruleMap of
    Just applicableRules -> 
      (doOrderRulesPass applicableRules currentPage pagesSeen)
      &&
      (isValidRequestOperator ruleMap pages (Set.insert currentPage pagesSeen))
    Nothing -> isValidRequestOperator ruleMap pages $ Set.insert currentPage pagesSeen


isValidRequest ruleMap (Pages pages) =
  isValidRequestOperator ruleMap pages Set.empty

partialParseRule :: [Lexer.Token] -> [Rule] -> ([Rule], [Lexer.Token])
partialParseRule tokens rules =
  case break isNewline tokens of
    ([Lexer.Word pgA, Lexer.Delimiter '|', Lexer.Word pgB], rest) ->
      partialParseRule (dropWhile isNewline rest) (rules ++ [OrderViolation (read pgA) (read pgB)])
    _ ->
      (rules, tokens)

partialParseRequests :: [Lexer.Token] -> [PrintRequest] -> ([PrintRequest], [Lexer.Token])
partialParseRequests tokens requests =
  case break isNewline tokens of
    ([], _) ->
      (requests, tokens)
    (requestLine, rest) ->
      let 
        (_, onlyNumbers) = List.partition isDelimiter requestLine
      in
        partialParseRequests (List.dropWhile isNewline rest) (requests ++ [Pages $ map (read . getString) $ onlyNumbers])

parseProblem tokens =
  let
    (rules, tokensRemaining) = partialParseRule tokens []
    (requests, _) = partialParseRequests tokensRemaining []
  in
    return (rules, requests)


programOptions :: IO (String, ())
programOptions =
  simpleOptions
    "0.1"
    "AOC 2024 Day 5 Solver"
    "Solves the day 5 problem for Advent of Code 2024."
    (strArgument $ metavar "FILEPATH")
    Options.Applicative.Simple.empty

isNewline :: Lexer.Token -> Bool
isNewline tok = 
  case tok of 
    Lexer.Newline -> True
    _ -> False

isDelimiter :: Lexer.Token -> Bool
isDelimiter tok = 
  case tok of 
    Lexer.Delimiter _ -> True
    _ -> False

getString :: Lexer.Token -> String
getString tok =
  case tok of
    Lexer.Word w -> w
    _ -> sorry "Expected a word, found something else."

instance Show Rule where
  showsPrec _ (OrderViolation pgA pgB) = showString $ (show pgA) ++ " BEFORE " ++ (show pgB)

instance Show PrintRequest where
  showsPrec _ (Pages pgs) = showString $ show pgs

makeRuleMap :: [Rule] -> Map.Map Int [Rule]
makeRuleMap list = 
  let
    ruleToMap (OrderViolation a b) = Map.singleton a [OrderViolation a b]
    mergeFold = merge preserveMissing preserveMissing (zipWithMatched (\_ a b -> a ++ b))
  in
    List.foldl mergeFold Map.empty (map ruleToMap list)

makeReverseRuleMap :: [Rule] -> Map.Map Int [Rule]
makeReverseRuleMap list = 
  let
    ruleToMap (OrderViolation a b) = Map.singleton b [OrderViolation a b]
    mergeFold = merge preserveMissing preserveMissing (zipWithMatched (\_ a b -> a ++ b))
  in
    List.foldl mergeFold Map.empty (map ruleToMap list)
