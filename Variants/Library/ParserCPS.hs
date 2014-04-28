{-# LANGUAGE OverloadedStrings, Rank2Types #-}
module Library.Parser where

import Prelude hiding (takeWhile)

--import Data.Char
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as ByteString

import Data.Either

import Control.Applicative
import Control.Monad

type Error = String

-- | Failure continuation.
type Failure f r   = Error -> f r
-- | Success continuation.
type Success a f r = a -> f r

-- | A continuation-based parser type.
newtype Parser a = Parser {
      runParser :: forall f r.
                   Failure f r
                -> Success a f r
                -> f r
    }

instance Functor Parser where
  f `fmap` p = Parser $ \kf ks ->
    let ks' a = ks (f a)
    in  runParser p kf ks'

instance Applicative Parser where
  pure a = Parser $ \_ ks -> ks a

  p <*> q = do
    f <- p
    a <- q
    return (f a)

instance Alternative Parser where
  empty = Parser $ \kf _ -> kf "empty"

  p <|> q = Parser $ \kf ks ->
    let kf' _ = runParser q kf ks
    in  runParser p kf' ks

instance Monad Parser where
  p >>= g = Parser $ \kf ks ->
    let ks' a = runParser (g a) kf ks
    in  runParser p kf ks'

  return = pure

  fail e = Parser $ \kf _ -> kf e


parse :: (a -> Parser b) -> a -> Either Error b
parse p v = runParser (p v) Left Right

satisfyWith :: (Char -> Bool) -> Parser a
satisfy p = Parser $ \kf ks ->
  s <- get
  case ByteString.uncons s of
    Just (c',s') -> if p c'
      then put s' >> return c'
      else fail "satisfy: no match"
    Nothing      -> fail "satisfy: empty string"

  do
  s <- ensure 1
  let c = f $! B.unsafeHead s
  if p c
    then let !t = B.unsafeTail s
         in put t >> return c
    else fail "satisfyWith"

{-
takeWhile :: (Char -> Bool) -> Parser ByteString
takeWhile t = Parser $ \s -> case ByteString.span t s of
  (p,s') -> Done s' p -- Never fails!

takeTill :: (Char -> Bool) -> Parser ByteString
takeTill t = takeWhile (not . t) -- Never fails!

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \s -> case ByteString.uncons s of
  Just (c',s') -> if p c'
                  then Done s' c'
                  else Fail s "char: no match"
  Nothing      -> Fail s "char: empty string"

char :: Char -> Parser Char
char c = satisfy (== c)

anyChar :: Parser Char
anyChar = satisfy (const True)

string :: ByteString -> Parser ByteString
string p = Parser $ \s -> if p `ByteString.isPrefixOf` s
  then Done (ByteString.drop (ByteString.length p) s) p
  else Fail s "string: no match"

endOfLine :: Parser ()
endOfLine = void (char '\n') <|> void (string "\r\n")

endOfInput :: Parser ()
endOfInput = Parser $ \s -> if ByteString.null s
  then Done s ()
  else Fail s "endOfInput: there is more"

choice :: [Parser a] -> Parser a
choice = foldr (<|>) empty

--word :: Parser ByteString
--word = takeWhile isLetter

--isEndOfLine :: Char -> Bool
--isEndOfLine c = c == '\n' || c == '\r'

--between :: Char -> Char -> Parser ByteString
--between o c = char o *> takeTill (== c) <* char c

parseOnly :: Parser a -> ByteString -> Either Error a
parseOnly p s = case runParser p s of
  Done _ a -> Right a
  Fail _ e -> Left e
  -}

