module Main where

import Data.Maybe

import Heffal.Lexer
import Heffal.Parser
import Heffal.Format.Cli
import Heffal.Format.Eww

main :: IO ()
main = do
    let tokens = strToTokens "# heading\n- this is a bullet\n\nAnd this is text\n\n# nice `heading` *very bold move*"
    let ast = tokensToAST tokens
    print tokens
    print ast
    putStr $ fromMaybe "" $ ast >>= (\f -> Just $ cliFmt f ())
    -- putStr $ fromMaybe "" $ ast >>= (\f -> Just $ ewwFmt f ())
