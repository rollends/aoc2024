import AOC2024.Support.Lexer as Lexer
import GHC.Utils.Panic (sorry)
import qualified Options.Applicative as Options
import qualified Options.Applicative.Simple as SimpleOptions
import System.IO

data Operator = Add | Multiply | Concatenate

data Equation = ProblemStatement Int [Int]

parseEquations :: [Lexer.Token] -> [Equation]
isSatisfiable :: Equation -> Bool
main :: IO ()
main =
  do
    (filepath, ()) <- programOptions
    handle <- openFile filepath ReadMode
    tokens <- lexFile (Just ":") handle
    print $ part1 $ parseEquations $ filter (not . isWhitespace) tokens
    return ()

part1 :: [Equation] -> Int
part1 equations =
  sum $ map getTotal $ filter isSatisfiable equations

parseEquationsStep :: [Lexer.Token] -> [Equation] -> [Equation]
parseEquationsStep [] problems = problems
parseEquationsStep tokens problems =
  let (line, otherLines) = break isNewline tokens
      (totalWord, remainderLine) = break isDelimiter line
   in case head totalWord of
        Lexer.Word strTotal ->
          let totalNumber = read strTotal
              numbers = map (read . getString) $ dropWhile isDelimiter remainderLine
           in parseEquationsStep (dropWhile isNewline otherLines) $ problems ++ [ProblemStatement totalNumber numbers]
        _ ->
          problems

parseEquations tokens = parseEquationsStep tokens []

isSatisfiableStep :: Equation -> Int -> Bool
isSatisfiableStep (ProblemStatement total values) accumulator =
  case values of
    value : remainingValues ->
      (accumulator <= total)
        && ( (isSatisfiableStep (ProblemStatement total remainingValues) (accumulator + value))
               || (isSatisfiableStep (ProblemStatement total remainingValues) (accumulator * value))
               || (isSatisfiableStep (ProblemStatement total remainingValues) $ read ((show accumulator) ++ (show value)))
           )
    [] -> accumulator == total

isSatisfiable eqn = isSatisfiableStep eqn 0

instance Show Operator where
  showsPrec _ Add = showString "+"
  showsPrec _ Multiply = showString "*"
  showsPrec _ Concatenate = showString "|"

instance Show Equation where
  showsPrec _ (ProblemStatement k list) = showString $ (show k) ++ " : " ++ show list

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

isWhitespace :: Lexer.Token -> Bool
isWhitespace tok =
  case tok of
    Lexer.Whitespace _ -> True
    _ -> False

getTotal :: Equation -> Int
getTotal (ProblemStatement k _) = k

getString :: Lexer.Token -> String
getString tok =
  case tok of
    Lexer.Word w -> w
    _ -> sorry "Expected a word, found something else."

programOptions :: IO (String, ())
programOptions =
  SimpleOptions.simpleOptions
    "0.1"
    "AOC 2024 Day 7 Solver"
    "Solves the day 7 problem for Advent of Code 2024."
    (Options.strArgument $ Options.metavar "FILEPATH")
    SimpleOptions.empty