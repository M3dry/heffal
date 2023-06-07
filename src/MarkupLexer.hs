module MarkupLexer where

import GeneralLexer
import TextLexer

data MarkupToken
    = TodoOpen
    | TodoClose
    | TodoState String
    | Bullet
    | Heading
    | Newline
    | Text [TextToken]
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
                            let (consumed, start) = consumeUntil lexer ']'
                             in addToken consumed $ TodoState $ take (current consumed - start) $ drop start (input consumed)
                    Just ']'
                        | case first of
                            TodoState _ -> True
                            _ -> False ->
                            addToken lexer TodoClose
                    Just '-' | head toks == Newline -> addToken lexer Bullet
                    Just '\n' -> addToken lexer Newline
                    _ ->
                        let (consumed, start) = consumeUntil (ignoreWhite lexer) '\n'
                            str = take (current consumed - start) $ drop start (input consumed)
                         in addToken consumed{current = current consumed - 1}
                                $ Text
                                    . getTokens
                                    . newLexer
                                $ str
