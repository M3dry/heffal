module Lexer where

data Token
    = TodoOpen
    | TodoClose
    | TodoState String
    | Bullet
    | Heading
    | Newline
    | Text [TextToken]

data TextToken
    = Pure String
    | Verbatim [TextToken]
    | Underline [TextToken]
    | Crossed [TextToken]
    | Bold [TextToken]
    | Italic [TextToken]

data Lexer = Lexer
    { input :: String
    , current :: Int
    , current_ch :: Maybe Char
    }

newLexer :: String -> Lexer
newLexer input =
    Lexer
        { input
        , current = 0
        , current_ch = Nothing
        }

readChar :: Lexer -> Lexer
readChar lexer = lexer
