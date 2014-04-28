{-# LANGUAGE NamedFieldPuns, DeriveDataTypeable #-}
module Main where

import Prelude hiding (takeWhile)

import Data.Char
import Data.List hiding (takeWhile)
import Data.Typeable

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Concurrent.Async

import System.Environment
import System.Exit

import Library.Output
import Library.SParser

-- | Een symbool is een abstract data type. Deze kunnen we goedkoper op de
-- stapel zetten dan strings. Een kleine optimalisatie dus. We leiden een
-- instantie voor @Eq@ en @Ord@ af en maken er zelf eentje voor @Show@ aan.
data Symbol  = Brace
             | Bracket
             | Paren
             | Chevron
             | Dollar
             | At
             | Delimiter
             | StartStop Name
             | BeginEnd  Name
             deriving (Show, Eq, Ord)
type Name    = String

openOf :: Symbol -> String
openOf s = case s of
  Brace       -> "{"
  Bracket     -> "["
  Paren       -> "("
  Chevron     -> "<"
  Dollar      -> "$"
  At          -> "@"
  Delimiter   -> "\\left"
  StartStop n -> "\\start" ++ n
  BeginEnd  n -> "\\begin{" ++ n ++ "}"

closeOf :: Symbol -> String
closeOf s = case s of
  Brace       -> "}"
  Bracket     -> "]"
  Paren       -> ")"
  Chevron     -> ">"
  Dollar      -> "$"
  At          -> "@"
  Delimiter   -> "\\right"
  StartStop n -> "\\stop" ++ n
  BeginEnd  n -> "\\end{" ++ n ++ "}"

-- Allereerst definiëren we een paar type-synoniemen. We gebruiken een
-- eenvoudige lijst als stapel (Stack), een foutmelding is een string en een
-- regelnummer is een geheel getal.
type Stack a = [a]
type Line    = Int
data Mode    = Normal | Math
             deriving (Show, Eq, Ord, Enum)
data State   = State { mode  :: Mode
                     , line  :: Line
                     , stack :: Stack (Line,Symbol)
                     } deriving (Show, Eq)

mkState :: State
mkState = State { mode  = Normal
                , line  = 1
                , stack = [] }

data BalancingError = EndOfFile Symbol Line
                    | DoesNotMatch Symbol Line Symbol Line
                    | ClosedWithoutOpening Symbol Line
                    deriving (Typeable)
instance Exception BalancingError

instance Show BalancingError where
  show e = case e of
    EndOfFile s' l'          ->                            "Unexpected end of file, expected '"                       ++ closeOf s' ++ "'\n   " ++ "(to close '" ++ openOf s' ++ "' from line " ++ show l' ++ ")"
    DoesNotMatch s l s' l'   -> "Line " ++ show l ++ ":\n   Unexpected '" ++ closeOf s ++ "', expected '" ++ closeOf s' ++ "'\n   " ++ "(to close '" ++ openOf s' ++ "' from line " ++ show l' ++ ")"
    ClosedWithoutOpening s l -> "Line " ++ show l ++ ":\n   Unexpected '" ++ closeOf s ++ "', closed without opening"

stripInfix :: Eq a => [a] -> [a] -> Maybe [a]
stripInfix [] l              = Just l
stripInfix _ []              = Nothing
stripInfix i l 
  | Just r <- stripPrefix i l = Just r
  | otherwise                = stripInfix i (tail l)

backslash, dollar, percent, at :: Parser Char
backslash = char '\\'
dollar    = char '$'
percent   = char '%'
at        = char '@'

between :: Char -> Char -> Parser String
between o c = char o *> takeTill (== c) <* char c

word :: Parser String
word = takeWhile isLetter

isEndOfLine :: Char -> Bool
isEndOfLine c = c == '\n' || c == '\r'

-- | Controleer of de string begint met een procent teken. In dat geval gooien
-- we alle tekens weg tot het einde van de regel, en geven de rest van de string
-- terug.
comment :: Parser String
comment = percent *> takeTill isEndOfLine -- Does not consume end of line character!

-- | Controleer of we in een woordelijke omgeving zitten. Alles tussen twee
-- apenstaartjes negeeren we.
verbatim :: Parser String
verbatim = between '@' '@'

-- | Controleer of de string begint met een geescaped procent teken, dollar
-- teken of apenstaartje.
-- We willen immers niet dat we over de backslash heen lezen, en vervolgens de
-- procent of het dollar teken aanzien voor commentaar dan wel wiskunde...
escaped :: Parser String
escaped = choice $ map string ["\\\\", "\\%", "\\$", "\\@"]

-- | Controleer of de string begint met een reeks van tekens dat we als haakje
-- kunnen gebruiken voor \left of \right. (Doen we op een creatieve manier
-- waarbij we optimaal gebruik maken van de luiheid van Haskell!)
delimiter :: Parser String
delimiter = choice $ map string
  [ ".", "(", ")", "[", "]", "<", ">", "|", "/"
  , "\\{", "\\}", "\\|"
  , "\\lgroup", "\\rgroup", "\\lbrace", "\\rbrace", "\\langle", "\\rangle"
  , "\\vert", "\\lvert", "\\rvert", "\\Vert", "\\lVert", "\\rVert"
  , "\\backslash", "\\lfloor", "\\rfloor", "\\lceil", "\\rceil"
  , "\\uparrow", "\\Uparrow", "\\downarrow", "\\Downarrow", "\\updownarrow", "\\Updownarrow"
  , "\\llcorner", "\\lrcorner", "\\ulcorner", "\\urconrner"
  , "\\lmoustache", "\\rmoustache" ]

math :: Parser Symbol
math = Dollar <$ dollar

-- | Controleer of de string begint met een openingshaakje, een start- of
-- begin-commando of met wiskunde. Als dat zo is, strippen we het deze tekst van
-- de string en geven we `gewoon' een paar terug met daarin het sluithaakje of
-- stop-commando en de rest van de string. Als we niets vinden, geven we `niets'
-- terug!
--
-- Het dollar teken is een vreemde eend in de bijt. We kunnen namelijk niet
-- controleren of dit de openings- of sluitings-dollar is. We houden in een
-- extra argument @m@ bij of we in wiskundemodus zitten of niet. Dit helpt ons
-- bij het onderscheid.
opening :: Parser Symbol
opening  =  Brace     <$  char '{'
        <|> Bracket   <$  char '['
        <|> Paren     <$  char '('
        <|> Delimiter <$  (string "\\left"   *> delimiter)
        <|> StartStop <$> (string "\\start"  *> word)
        <|> BeginEnd  <$> (string "\\begin{" *> word <* char '}')

-- | Analoog aan @opening@, maar dan voor sluithaakjes en stop-commando's.
closing :: Parser Symbol
closing  =  Brace     <$  char '}'
        <|> Bracket   <$  char ']'
        <|> Paren     <$  char ')'
        <|> Delimiter <$  (string "\\right" *> delimiter)
        <|> StartStop <$> (string "\\stop"  *> word)
        <|> BeginEnd  <$> (string "\\end{"  *> word <* char '}')

-- | Controleer of de string begint met een nieuwe regel. Strip deze en geef de
-- rest van de string terug.
newLine :: String -> Maybe String
newLine = stripPrefix "\n"

{-
-- | Recursief algoritme om te controleren of de string gebalanceerd is
-- wat betreft haakjes en start/stop-paren.
--
-- Een korte omschrijving per regel:
-- * Een lege string met een lege stapel is natuurlijk gebalanceerd.
-- * Een lege string met een niet lege stapel gaat helemaal mis.
-- * In andere gevallen:
--   * We houden het regelnummer bij en hogen het op als we een regeleinde
--     tegenkomen.
--   * We controleren of we met een \% of \$ te maken hebben, dat gooien we weg
--     zodat we verderop niet in de problemen komen.
--   * Commentaar gooien we weg.
--   * Wanneer we een openingshaakje tegenkomen zetten we het huidige
--     regelnummer en het sluithaakje op de stapel.
--   * Wanneer we een sluithaakje tegenkomen controleren we of dit op de stapel
--     stond.
--   * Als de string niet begint met een haakje of start/stop-paar, dan gaan we
--     door met de rest van de tekst.
--
-- Het type van deze functie vraagt eigenlijk om een Writer Monad, maar omdat
-- dit maar een script is, gaan we het ons niet te ingewikkeld maken...
balanced :: Parser Bool
balanced = go mkState where
    go st  =  (endOfLine  >>  go $ increase st)
          <|> (escaped    >>  go st)
          <|> (comment    >>  go st)
          <|> (verbatim   >>  go st)
          <|> (math       >>= go $ decide st )
          <|> (opening    >>= go $ push st)
          <|> (closing    >>= go $ pop st)
          <|> (anyChar    >>  go st)
          <|> (endOfInput >>  end st)
          -- <?> "something went wrong"
    -- Just (d,r) <- verbatim s = uncurry go $ skip r d st

end :: Monad m => State -> m Bool
end State{stack=(line,open):_,line} = fail $ "Line " ++ show line ++ ":\n   " ++ "Unexpected end of file, expected '" ++ closeOf open ++ "'\n   " ++ "(to match with '" ++ openOf open ++ "' from line " ++ show line ++ ")"
end _                                   = return True

end :: State -> Bool
end st = case stack st of
  []            -> True
  (line,open):_ -> throw $ EndOfFile open line

increase :: State -> State
increase st = st{line = line st + 1}

decide :: Symbol -> State -> State
decide open st = case mode st of
  Math   -> pop  open st{mode = Normal}
  Normal -> push open st{mode = Math}

push :: Symbol -> State -> State
push open st = st{stack = (line st, open) : stack st}

pop :: Symbol -> State -> State
pop close st@State{stack,line} = case stack of
  []                   -> throw $ ClosedWithoutOpening close line
  (line',open):rest
    | close == open    -> st{stack = rest}
    | otherwise        -> throw $ DoesNotMatch close line open line'

skip :: String -> Symbol -> State -> (String,State)
skip s open st@State{line}
  | Just r <- stripInfix (closeOf open) s = (r, st)
  | otherwise                          = throw $ EndOfFile open line

-- | Lees de tekst in uit een bestand, controleer of deze gebalanceerd is en
-- geef het resultaat door.
run :: FilePath -> IO Bool
run f = do
  putAct f
  s <- readFile f
  evaluate (balanced s) `catch` \e -> do
    putErr $ show (e :: BalancingError)
    return False

-- | Hoofdfunctie waarbij we de commandoregel argumenten inlezen en controleren
-- of die er überhaupt wel zijn. Vervolgens passen we @run@ toe op elk argument
-- en geven nog een conclusie of het aantal fouten.
main :: IO a
main = do
  as <- getArgs
  when (null as) $ do
    putWrn "Without arguments I can't do anything!"
    exitFailure

  rs <- mapM run as
  let n = length . filter (== False) $ rs
  case n of
    0 -> putInf "Everything seems to be all right!"
    1 -> putInf "Oops, found 1 error!"
    _ -> putInf $ "Oh help, found " ++ show n ++ " errors!"
  exitSuccess
-}

balanced :: Parser Bool
balanced = go mkState where
    go st  =  (endOfLine  >>  increase st >>= go)
          <|> (escaped    >>  go st)
          <|> (comment    >>  go st)
          <|> (verbatim   >>  go st)
          <|> (math       >>= decide st >>= go)
          <|> (opening    >>= push st >>= go)
          <|> (closing    >>= pop st >>= go)
          <|> (anyChar    >>  go st)
          <|> (endOfInput >>  end st)
          -- <?> "something went wrong"

end :: Monad m => State -> m Bool
end State{stack=(line',open):_,line} = fail $ "Line " ++ show line ++ ":\n   " ++ "Unexpected end of file, expected '" ++ closeOf open ++ "'\n   " ++ "(to match with '" ++ openOf open ++ "' from line " ++ show line ++ ")"
end _                                   = return True

increase :: Monad m => State -> m State
increase state@State{line} = return state{line = line + 1}

decide :: Monad m => State -> Symbol -> m State
decide state symbol
  | mode state == Math = pop state{mode = Normal} symbol
  | otherwise    = push state{mode = Math} symbol

push :: Monad m => State -> Symbol -> m State
push state@State{stack,line} open = return state{stack = (line, open) : stack}

pop :: Monad m => State -> Symbol -> m State
pop state@State{stack,line} close = case stack of
  []                   -> fail $ "Line " ++ show line ++ ":\n   " ++ "Unexpected '" ++ closeOf close ++ "', closed without opening"
  (line,open):rest
    | close == open    -> return state{stack = rest}
    | otherwise        -> fail $ "Line " ++ show line ++ ":\n   " ++ "Unexpected '" ++ closeOf close ++ "', expected '" ++ closeOf open ++ "'\n   " ++ "(to match with '" ++ openOf open ++ "' from line " ++ show line ++ ")"

-- | Lees de tekst in uit een bestand, controleer of deze gebalanceerd is en
-- geef het resultaat door.
run :: FilePath -> IO Bool
run f = do
  putAct f
  t <- readFile f
  case parseOnly balanced t of
    Right bool -> return bool
    Left  mesg -> putErr mesg >> return False

-- | Hoofdfunctie waarbij we de commandoregel argumenten inlezen en controleren
-- of die er überhaupt wel zijn. Vervolgens passen we @run@ toe op elk argument
-- en geven nog een conclusie of het count fouten.
main :: IO a
main = do
  as <- getArgs
  when (null as) $ do
    putWrn "Without arguments I can't do anything!"
    exitFailure

  rs <- mapConcurrently run as
  let n = length . filter (== True) $ rs
  case n of
    0 -> putInf "Everything seems to be all right!"
    1 -> putInf "Oops, found 1 error!"
    _ -> putInf $ "Oh help, found " ++ show n ++ " errors!"
  exitSuccess

-- vim: nowrap
