module TextLexer where

import GeneralLexer

data TextToken
    = Pure String
    | Verbatim [TextToken]
    | Underline [TextToken]
    | Crossed [TextToken]
    | Bold [TextToken]
    | Italic [TextToken]
    deriving (Show, Eq)

instance Token TextToken where
    nextToken lexer = lexer{current = length $ input lexer, current_ch = Nothing, tokens = [Pure $ input lexer]}
