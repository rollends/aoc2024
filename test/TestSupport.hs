import AOC2024.Support.Lexer as Lexer
import Test.HUnit

testSingleWord :: Test
testSingleWord = 
  TestCase $
    assertEqual "Single Word"
    [Lexer.Word "word"]
    (lexString Nothing "word")

testMultipleSpaces :: Test
testMultipleSpaces =
  TestCase $
    assertEqual "Multiple Spaces"
    [Lexer.Whitespace 4]
    (lexString Nothing "    ")

testNewlineLineFeed :: Test
testNewlineLineFeed =
  TestCase $
    assertEqual "Linefeed"
    [Lexer.Newline]
    (lexString Nothing "\n\n\n\n")

testNewlineCR :: Test
testNewlineCR =
  TestCase $
    assertEqual "Carriage Return"
    [Lexer.Newline]
    (lexString Nothing "\r\r\r\r")

testNewlineCRLF :: Test
testNewlineCRLF =
  TestCase $
    assertEqual "Windows Newline"
    [Lexer.Newline]
    (lexString Nothing "\r\n\r\n")

testDelimiter :: Test
testDelimiter =
  TestCase $
    assertEqual "Delimiter"
    [Lexer.Delimiter, Lexer.Word "d", Lexer.Delimiter]
    (lexString (Just 'c') "ccdc")

testBasicLexing :: Test
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

testSmallExample :: Test
testSmallExample =
  TestCase $
    assertEqual "Small Text Body"
    [
      Lexer.Word "3", Lexer.Whitespace 1, Lexer.Word "4", Lexer.Newline,
      Lexer.Word "4", Lexer.Whitespace 3, Lexer.Word "3", Lexer.Newline,
      Lexer.Word "2", Lexer.Whitespace 3, Lexer.Word "5", Lexer.Newline
    ]
    (lexString Nothing "3 4\n4   3\n2   5\n")

testCSVExample :: Test
testCSVExample =
  TestCase $
    assertEqual "Small CSV"
    [
      Lexer.Word "3", Lexer.Delimiter, Lexer.Word "4", Lexer.Newline,
      Lexer.Word "4", Lexer.Delimiter, Lexer.Whitespace 2, Lexer.Word "3", Lexer.Newline,
      Lexer.Word "2", Lexer.Whitespace 2, Lexer.Delimiter, Lexer.Whitespace 1, Lexer.Word "5", Lexer.Newline
    ]
    (lexString (Just ',') "3,4\n4,  3\n2  , 5\n")

testSimpleExamples :: Test
testSimpleExamples = TestList [testSmallExample, testCSVExample]
  
tests :: Test
tests = 
  TestList [
    TestLabel "Basic Lexing" testBasicLexing,
    TestLabel "Simple Examples" testSimpleExamples
  ]

main :: IO ()
main = runTestTTAndExit tests