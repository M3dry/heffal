module TextLexer where

import Helper
import GeneralLexer

data TextToken
    = Pure String
    | Verbatim [TextToken]
    | Underline [TextToken]
    | Crossed [TextToken]
    | Bold [TextToken]
    | Italic [TextToken]
    | Illegal
    deriving (Show, Eq)

instance Token TextToken where
    nextToken lexer = case current_ch lexer of 
                        Just '`' -> textModifier lexer '`' Verbatim
                        Just '_' -> textModifier lexer '_' Underline
                        Just '-' -> textModifier lexer '-' Crossed
                        Just '*' -> textModifier lexer '*' Bold
                        Just '/' -> textModifier lexer '/' Italic
                        -- Just _ -> lexer{current = length $ input lexer, current_ch = Nothing, tokens = [Pure $ input lexer]}
                        Just _ -> let (consumed, start, _) = consumeUntil lexer ( == '-')
                                      str = take (current consumed - start) $ drop start (input consumed)
                         in addToken consumed $ Pure str
                        _ -> addToken lexer Illegal


textModifier :: Lexer TextToken -> Char -> ([TextToken] -> TextToken) -> Lexer TextToken
textModifier lexer ch token =
     let (consumed, start, found) = consumeUntil (readChar lexer) (\ch' -> ch' == ch || ch' == '\n')
         in if justIs found (==ch)
            then addToken (readChar consumed) $ token . getTokens . newLexer $ subStr (input consumed) start (current consumed)
             else case getTokens . newLexer $ subStr (input consumed) start (current consumed)  of
                    (Pure str:xs) -> addTokens consumed (Pure (ch : str):xs)
                    [] -> consumed
                    xs -> addTokens consumed (Pure [ch] : xs)