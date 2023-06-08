module Heffal.GeneralLexer where

import Heffal.Helper
import Data.Char (isSpace)
import Data.Maybe

data Lexer a = Lexer
    { input :: String
    , current :: Int
    , current_ch :: Maybe Char
    , tokens :: [a]
    }
    deriving (Show, Eq)

class Token a where
    nextToken :: Lexer a -> Lexer a

newLexer :: (Token a) => String -> Lexer a
newLexer str =
    readChar
        Lexer
            { input = str
            , current = -1
            , current_ch = Nothing
            , tokens = []
            }

readChar :: (Token a) => Lexer a -> Lexer a
readChar lexer =
    let cur = current lexer + 1
     in lexer
            { current = cur
            , current_ch =
                if length (input lexer) > cur
                    then Just (input lexer !! cur)
                    else Nothing
            }

peekChar :: (Token a) => Lexer a -> Maybe Char
peekChar Lexer{current = current, input = input} =
    if length input > current + 1
        then Just $ input !! (current + 1)
        else Nothing

ignoreWhite :: (Token a) => Lexer a -> Lexer a
ignoreWhite lexer =
    if justIs (current_ch lexer) isSpace
        then ignoreWhite $ readChar lexer
        else lexer

addToken :: (Token a) => Lexer a -> a -> Lexer a
addToken lexer token = lexer{tokens = token : tokens lexer}

addTokens :: (Token a) => Lexer a -> [a] -> Lexer a
addTokens = foldl addToken

consumeUntil :: (Token a) => Lexer a -> (Char -> Bool) -> (Lexer a, Int, Maybe Char)
consumeUntil lexer ch = let (consumed, start, found) = consumePeekUntil lexer ch in (readChar consumed, start, found)

consumePeekUntil :: (Token a) => Lexer a -> (Char -> Bool) -> (Lexer a, Int, Maybe Char)
consumePeekUntil lexer ch =
    let start = current lexer
        (consumed, found) = recur lexer ch
     in (consumed, start, found)
  where
    recur lexer' ch'
        | isNothing $ current_ch lexer' = (lexer', Nothing)
        | justIs (peekChar lexer') ch' = (lexer', peekChar lexer')
        | otherwise = recur (readChar lexer') ch'

getTokens :: (Token a) => Lexer a -> [a]
getTokens = reverse . tokens . recur
  where
    recur lexer' =
        if isNothing $ current_ch lexer'
            then lexer'
            else recur $ nextToken lexer'
