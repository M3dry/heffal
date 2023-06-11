module Heffal.Format.Cli (cliFmt) where

import Heffal.Lexer (TextToken (..))
import Heffal.Parser

class CliFmt a c where
    cliFmt :: a -> c -> String

instance CliFmt File () where
    cliFmt (File []) _ = ""
    cliFmt (File (x : xs)) context =
        cliFmt x context ++ "\n" ++ cliFmt (File xs) context

instance CliFmt Heading () where
    cliFmt (Heading{name, contents}) context =
        "Heading "
            ++ cliFmt name context
            ++ cliFmt contents context
            ++ "\n"

instance CliFmt [HeadingContent] () where
    cliFmt contents context =
        foldl (\acc c -> acc ++ "\n" ++ cliFmt c context) "" contents

instance CliFmt HeadingContent () where
    cliFmt hContent context = case hContent of
        Text text -> "  " ++ cliFmt text context
        Bullet text -> "  - " ++ cliFmt text context
        Todo{state, text} -> "  [" ++ state ++ "] " ++ cliFmt text context

instance CliFmt [TextToken] () where
    cliFmt tokens context = foldl (\acc t -> acc ++ cliFmt t context) "" tokens

instance CliFmt TextToken () where
    cliFmt (Pure str) _ = str
    cliFmt (Verbatim text) context =
        textFmt '`' text context
    cliFmt (Underline text) context =
        textFmt '_' text context
    cliFmt (Crossed text) context =
        textFmt '-' text context
    cliFmt (Bold text) context =
        textFmt '*' text context
    cliFmt (Italic text) context =
        textFmt '/' text context

textFmt :: Char -> [TextToken] -> () -> String
textFmt ch text context = ch : cliFmt text context ++ [ch]
