import AOC2024.Support.Lexer as Lexer
import Options.Applicative
import Options.Applicative.Simple
import System.IO
import Text.Regex.TDFA

part1 :: [Lexer.Token] -> Int
part1_accumulator :: [Lexer.Token] -> Int -> Int
evaluate_mul_string :: String -> Int
part2 :: [Lexer.Token] -> Int
part2_accumulator :: [Lexer.Token] -> Bool -> Int -> Int

part1_accumulator [] accum = accum
part1_accumulator (current : rest) accum =
  let regex_pattern = "mul[(][0-9]+,[0-9]+[)]"
      part1_accumulator_recursion = part1_accumulator rest
   in case current of
        Word string ->
          part1_accumulator_recursion $
            accum
              + (sum $ map evaluate_mul_string $ (getAllTextMatches (string =~ regex_pattern)))
        _ ->
          part1_accumulator_recursion accum

part2_accumulator [] _ accum = accum
part2_accumulator ((Word []) : rest) b accum = part2_accumulator rest b accum
part2_accumulator (Delimiter : rest) b accum = part2_accumulator rest b accum
part2_accumulator (Newline : rest) b accum = part2_accumulator rest b accum
part2_accumulator ((Whitespace _) : rest) b accum = part2_accumulator rest b accum
part2_accumulator ((Word string) : rest) True accum =
  let regex_dont_pattern = "don[']t[(][)]"
      (beforedont, foundText, afterdont) = string =~ regex_dont_pattern :: (String, String, String)
   in case foundText of
        "" -> part2_accumulator rest True (part1_accumulator [Word beforedont] accum)
        _ -> part2_accumulator ([Word afterdont] ++ rest) False (part1_accumulator [Word beforedont] accum)
part2_accumulator ((Word string) : rest) False accum =
  let regex_do_pattern = "do[(][)]"
      (_, foundText, afterdo) = string =~ regex_do_pattern :: (String, String, String)
   in case foundText of
        "" -> part2_accumulator rest False accum
        _ -> part2_accumulator ([Word afterdo] ++ rest) True accum

part1 list = part1_accumulator list 0

part2 list = part2_accumulator list True 0

main :: IO ()
main =
  do
    (filepath, ()) <- programOptions
    handle <- openFile filepath ReadMode
    tokens <- lexFile Nothing handle
    print $ part1 tokens
    print $ part2 tokens
    return ()

programOptions :: IO (String, ())
programOptions =
  simpleOptions
    "0.1"
    "AOC 2024 Day 3 Solver"
    "Solves the day 3 problem for Advent of Code 2024."
    (strArgument $ metavar "FILEPATH")
    Options.Applicative.Simple.empty

evaluate_mul_string string =
  let regex_pattern = "[0-9]+"
      multiply_ints = product . (map read)
   in multiply_ints (getAllTextMatches (string =~ regex_pattern))
