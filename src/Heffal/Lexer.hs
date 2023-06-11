module Heffal.Lexer (strToTokens, MarkupToken(..), TextToken(..)) where

import Heffal.GeneralLexer
import Heffal.Helper

data MarkupToken
    = TodoOpen
    | TodoClose
    | TodoState String
    | Bullet
    | Heading
    | Newline
    | Text [TextToken]
    | Illegal
    deriving (Show, Eq)

instance Token MarkupToken where
    nextToken lexer =
        let toks = tokens lexer
            empty = null toks
            first = head toks
         in readChar $
                case current_ch lexer of
                    Just '#' | empty || (first == Newline) -> addToken lexer Heading
                    Just '[' | first == Newline -> addToken lexer TodoOpen
                    Just _
                        | not empty && first == TodoOpen ->
                            let (consumed, start, found) = consumePeekUntil lexer (== ']')
                             in addToken consumed $ if justIs found (== ']') then TodoState $ subStr (input consumed) start (current consumed) else Illegal
                    Just ']'
                        | case first of
                            TodoState _ -> True
                            _ -> False ->
                            addToken lexer TodoClose
                    Just '-' | head toks == Newline -> addToken lexer Bullet
                    Just '\n' -> addToken lexer Newline
                    _ ->
                        let (consumed, start, _) = consumeUntil (ignoreWhite lexer) (== '\n')
                            str = take (current consumed - start) $ drop start (input consumed)
                         in addToken consumed{current = current consumed - 1}
                                $ Text
                                    . getTokens
                                    . newLexer
                                $ str

data TextToken
    = Pure String
    | Verbatim [TextToken]
    | Underline [TextToken]
    | Crossed [TextToken]
    | Bold [TextToken]
    | Italic [TextToken]
    deriving (Show, Eq)

instance Token TextToken where
    nextToken lexer = case current_ch lexer of
        Just '`' -> textModifier lexer '`' Verbatim
        Just '_' -> textModifier lexer '_' Underline
        Just '-' -> textModifier lexer '-' Crossed
        Just '*' -> textModifier lexer '*' Bold
        Just '/' -> textModifier lexer '/' Italic
        Just _ ->
            let (consumed, start, _) = consumeUntil lexer (`elem` ['`', '_', '-', '*', '/'])
                str = take (current consumed - start) $ drop start (input consumed)
             in addToken consumed $ Pure str
        _ -> lexer

textModifier :: Lexer TextToken -> Char -> ([TextToken] -> TextToken) -> Lexer TextToken
textModifier lexer ch token =
    let (consumed, start, found) = consumeUntil (readChar lexer) (\ch' -> ch' == ch || ch' == '\n')
        strToks = getTokens . newLexer $ subStr (input consumed) start (current consumed)
     in if justIs found (== ch)
            then addToken (readChar consumed) $ token strToks
            else case strToks of
                (Pure str : xs) -> addTokens consumed (Pure (ch : str) : xs)
                [] -> consumed
                xs -> addTokens consumed (Pure [ch] : xs)

strToTokens :: String -> [MarkupToken]
strToTokens = getTokens . newLexer
