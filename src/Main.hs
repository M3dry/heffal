module Main where

import Data.Maybe
import GeneralLexer
import MarkupLexer

main :: IO ()
main = do
    print . getTokens . (newLexer :: String -> Lexer MarkupToken) $ "\n[www]   hello text\n- nice"
  where
    recur lexer
        | isNothing (current_ch lexer) = []
        | otherwise = fromJust (current_ch lexer) : recur (readChar lexer)
