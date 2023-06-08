module Main where

import Data.Maybe
import Heffal.GeneralLexer
import Heffal.MarkupLexer

main :: IO ()
main = do
    print . getTokens . (newLexer :: String -> Lexer MarkupToken) $ "# heading\n[x] todo complete\n- bullet - /*bold+*italic/"
  where
    recur lexer
        | isNothing (current_ch lexer) = []
        | otherwise = fromJust (current_ch lexer) : recur (readChar lexer)
