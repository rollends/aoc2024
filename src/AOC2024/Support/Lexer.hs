module AOC2024.Support.Lexer (lexFile, lexString, Token (..)) where

import Data.Char
import GHC.IO.Encoding.UTF8 (utf8)
import GHC.IO.Handle

data Token
  = Whitespace Int
  | Newline
  | Word String
  | Delimiter Char

instance Eq Token where
  (==) (Whitespace a) (Whitespace b) = (a == b)
  (==) (Word a) (Word b) = (a == b)
  (==) Newline Newline = True
  (==) (Delimiter c) (Delimiter d) = c == d
  (==) _ _ = False

instance Show Token where
  showsPrec _ (Whitespace _) = showString " WS "
  showsPrec _ (Delimiter c) = showString $ " SEP '" ++ [c] ++ "' "
  showsPrec _ (Word a) = showString $ " '" ++ a ++ "' "
  showsPrec _ Newline = showString " NL "

data LexerState = LexerState [Token] (Maybe Token)

initialLexerState :: LexerState
popToken :: LexerState -> LexerState
pushToken :: LexerState -> Token -> LexerState
completeLex :: LexerState -> [Token]
lexStringFoldOperation :: Maybe String -> LexerState -> Char -> LexerState
lexStringIdentifyToken :: Maybe String -> LexerState -> Char -> GeneralCategory -> LexerState
lexString :: Maybe String -> [Char] -> [Token]
lexFile :: Maybe String -> Handle -> IO [Token]
lexFile delim handle =
  do
    hSetEncoding handle utf8
    hSetNewlineMode handle universalNewlineMode
    fmap (lexString delim) $ hGetContents handle
initialLexerState = LexerState [] Nothing

popToken (LexerState v Nothing) = LexerState v Nothing
popToken (LexerState v (Just t)) = LexerState (v ++ [t]) Nothing

pushToken state token =
  let LexerState v _ = popToken state
   in LexerState v $ Just token

completeLex (LexerState v (Just a)) = completeLex $ popToken $ (LexerState v (Just a))
completeLex (LexerState v Nothing) = v

lexStringIdentifyToken _ state _ Space =
  case state of
    LexerState v (Just (Whitespace a)) -> LexerState v $ Just $ Whitespace $ a + 1
    _ -> pushToken state $ Whitespace 1
lexStringIdentifyToken _ state _ Control =
  case state of
    LexerState _ (Just Newline) -> state
    _ -> pushToken state $ Newline
lexStringIdentifyToken Nothing state c _ =
  case state of
    LexerState v (Just (Word s)) -> LexerState v $ Just $ Word $ s ++ [c]
    _ -> pushToken state $ Word [c]
lexStringIdentifyToken (Just delim) state c typ =
  case (c `elem` delim, state) of
    (True, LexerState v (Just (Delimiter sc))) -> 
      if c == sc then 
        LexerState v $ Just (Delimiter sc) 
      else
        pushToken state $ Delimiter c
    (True, LexerState _ _) -> pushToken state $ Delimiter c
    (False, _) -> lexStringIdentifyToken Nothing state c typ

lexStringFoldOperation delim state c = lexStringIdentifyToken delim state c $ generalCategory c

lexString delim = completeLex . (foldl (lexStringFoldOperation delim) initialLexerState)