module GeneralLexer where

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

ignoreWhite :: (Token a) => Lexer a -> Lexer a
ignoreWhite lexer =
    -- I just learned about monads :P
    if isJust $ current_ch lexer >>= (\ch -> if isSpace ch then Just () else Nothing)
        then ignoreWhite $ readChar lexer
        else lexer

addToken :: (Token a) => Lexer a -> a -> Lexer a
addToken lexer token = lexer{tokens = token : tokens lexer}

consumeUntil :: (Token a) => Lexer a -> Char -> (Lexer a, Int)
consumeUntil lexer ch = let start = current lexer in (recur lexer ch, start)
  where
    recur lexer' ch'
        | isNothing $ current_ch lexer' = lexer'
        | fromJust (current_ch lexer') == ch' = lexer'
        | otherwise = recur (readChar lexer') ch'

getTokens :: (Token a) => Lexer a -> [a]
getTokens lexer = tokens . recur $ lexer
  where
    recur lexer =
        if isNothing $ current_ch lexer
            then lexer
            else recur $ nextToken lexer
