module AOC2024.Support.Lexer (lexFile, lexString, Token (..)) where

import Data.Char
import Data.List
import GHC.IO.Encoding.UTF8 (utf8)
import GHC.IO.Handle

data Token
  = Whitespace Int
  | Newline
  | Word String
  | Delimiter

instance Eq Token where
  (==) (Whitespace a) (Whitespace b) = (a == b)
  (==) (Word a) (Word b) = (a == b)
  (==) Newline Newline = True
  (==) Delimiter Delimiter = True
  (==) _ _ = False

instance Show Token where
  showsPrec _ (Whitespace _) = showString " WS "
  showsPrec _ Delimiter = showString " SEP "
  showsPrec _ (Word a) = showString $ " '" ++ a ++ "' "
  showsPrec _ Newline = showString " NL "

data LexerState = LexerState [Token] (Maybe Token)

initialLexerState :: LexerState
popToken :: LexerState -> LexerState
mapToken :: LexerState -> (Token -> Token) -> LexerState
pushToken :: LexerState -> Token -> LexerState
completeLex :: LexerState -> [Token]
lexStringFoldOperation :: Maybe Char -> LexerState -> Char -> LexerState
lexStringIdentifyToken :: Maybe Char -> LexerState -> Char -> GeneralCategory -> LexerState
lexString :: Maybe Char -> [Char] -> [Token]
lexFile :: Maybe Char -> Handle -> IO [Token]
lexFile delim handle =
  do
    hSetEncoding handle utf8
    hSetNewlineMode handle universalNewlineMode
    fmap (lexString delim) $ hGetContents handle
initialLexerState = LexerState [] Nothing

popToken (LexerState v Nothing) = LexerState v Nothing
popToken (LexerState v (Just t)) = LexerState (v ++ [t]) Nothing

mapToken (LexerState v m) op = LexerState v (fmap op m)

pushToken state token =
  let LexerState v _ = popToken state
   in LexerState v $ Just token

completeLex (LexerState v (Just a)) = completeLex $ popToken $ (LexerState v (Just a))
completeLex (LexerState v Nothing) = v

lexStringIdentifyToken _ state c Space =
  case state of
    LexerState v (Just (Whitespace a)) -> LexerState v $ Just $ Whitespace $ a + 1
    _ -> pushToken state $ Whitespace 1
lexStringIdentifyToken _ state c Control =
  case state of
    LexerState v (Just Newline) -> state
    _ -> pushToken state $ Newline
lexStringIdentifyToken Nothing state c _ =
  case state of
    LexerState v (Just (Word s)) -> LexerState v $ Just $ Word $ s ++ [c]
    _ -> pushToken state $ Word [c]
lexStringIdentifyToken (Just delim) state c typ =
  case (c == delim, state) of
    (True, LexerState v (Just Delimiter)) -> LexerState v $ Just Delimiter
    (True, LexerState v _) -> pushToken state $ Delimiter
    (False, _) -> lexStringIdentifyToken Nothing state c typ

lexStringFoldOperation delim state c = lexStringIdentifyToken delim state c $ generalCategory c

lexString delim = completeLex . (foldl (lexStringFoldOperation delim) initialLexerState)