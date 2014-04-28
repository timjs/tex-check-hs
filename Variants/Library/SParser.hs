{-# LANGUAGE OverloadedStrings #-}
module Library.SParser where

import Prelude hiding (takeWhile)

-- import Data.Char
import Data.List hiding (takeWhile)

import Control.Applicative
import Control.Monad

newtype Parser a = Parser { runParser :: String -> Result a }
data    Result a = Done String a
                 | Fail String Error
type    Error    = String

instance Functor Parser where
  f `fmap` Parser p = Parser $ \s -> case p s of
    Done s' a -> Done s' (f a)
    Fail s' e -> Fail s' e

instance Applicative Parser where
  pure a = Parser $ \s -> Done s a

  Parser p <*> Parser q = Parser $ \s -> case p s of
    Done s' f -> case q s' of
      Done s'' a -> Done s'' (f a)
      Fail s'' e -> Fail s'' e
    Fail s' e -> Fail s' e

instance Alternative Parser where
  empty = Parser $ \s -> Fail s ""

  Parser p <|> Parser q = Parser $ \s -> case p s of
    Fail _ _ -> q s -- Backtracking!
    done     -> done

instance Monad Parser where
  return a = Parser $ \s -> Done s a -- == pure

  -- :: (String -> Result a) -> (a -> String -> Result a) -> String -> Result a
  Parser p >>= f = Parser $ \s -> case p s of
    Done s' a -> case f a of
      Parser q -> q s'
    Fail s' e -> Fail s' e

  fail e = Parser $ \s -> Fail s e

takeWhile :: (Char -> Bool) -> Parser String
takeWhile t = Parser $ \s -> case span t s of
  (p,s') -> Done s' p -- Never fails!

takeTill :: (Char -> Bool) -> Parser String
takeTill t = takeWhile (not . t) -- Never fails!

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \s -> case s of
  (c:s') -> if p c
                  then Done s' c
                  else Fail s "char: no match"
  []     -> Fail s "char: empty string"

char :: Char -> Parser Char
char c = satisfy (== c)

anyChar :: Parser Char
anyChar = satisfy (const True)

string :: String -> Parser String
string p = Parser $ \s -> if p `isPrefixOf` s
  then Done (drop (length p) s) p
  else Fail s "string: no match"

endOfLine :: Parser ()
endOfLine = void (char '\n') <|> void (string "\r\n")

endOfInput :: Parser ()
endOfInput = Parser $ \s -> if null s
  then Done s ()
  else Fail s "endOfInput: there is more"

choice :: [Parser a] -> Parser a
choice = foldr (<|>) empty

manyTill :: Parser a -> Parser b -> Parser [a]
manyTill p end = scan
    where scan = (end *> pure []) <|> liftA2 (:) p scan

--word :: Parser String
--word = takeWhile isLetter

--isEndOfLine :: Char -> Bool
--isEndOfLine c = c == '\n' || c == '\r'

--between :: Char -> Char -> Parser String
--between o c = char o *> takeTill (== c) <* char c

parseOnly :: Parser a -> String -> Either Error a
parseOnly p s = case runParser p s of
  Done _ a -> Right a
  Fail _ e -> Left e

