import AOC2024.Support.Lexer as Lexer
import Test.HUnit

testSingleWord = 
  TestCase $
    assertEqual "Single Word"
    [Lexer.Word "word"]
    (lexString Nothing "word")

testMultipleSpaces =
  TestCase $
    assertEqual "Multiple Spaces"
    [Lexer.Whitespace 4]
    (lexString Nothing "    ")

testNewlineLineFeed =
  TestCase $
    assertEqual "Linefeed"
    [Lexer.Newline]
    (lexString Nothing "\n\n\n\n")

testNewlineCR =
  TestCase $
    assertEqual "Carriage Return"
    [Lexer.Newline]
    (lexString Nothing "\r\r\r\r")

testNewlineCRLF =
  TestCase $
    assertEqual "Windows Newline"
    [Lexer.Newline]
    (lexString Nothing "\r\n\r\n")

testDelimiter =
  TestCase $
    assertEqual "Delimiter"
    [Lexer.Delimiter, Lexer.Word "d", Lexer.Delimiter]
    (lexString (Just 'c') "ccdc")

testBasicLexing = 
  TestList 
  [
    testSingleWord, 
    testMultipleSpaces,
    testNewlineLineFeed,
    testNewlineCR,
    testNewlineCRLF,
    testDelimiter
  ]
tests = TestList [TestLabel "Basic Lexing" testBasicLexing]

main :: IO ()
main = runTestTTAndExit tests