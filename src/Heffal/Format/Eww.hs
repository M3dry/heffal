module Heffal.Format.Eww (ewwFmt) where

import Heffal.Lexer (TextToken (..))
import Heffal.Parser

class EwwFmt a c where
    ewwFmt :: a -> c -> String

instance EwwFmt File () where
    ewwFmt (File []) _ = ""
    ewwFmt (File (x:xs)) context =
        ewwFmt x context ++ ewwFmt (File xs) context

instance EwwFmt Heading () where
    ewwFmt (Heading{name, contents}) context = ""

instance EwwFmt [HeadingContent] () where
    ewwFmt contents context =
        foldl (\acc c -> acc ++ ewwFmt c context) "" contents

instance EwwFmt HeadingContent () where
    ewwFmt hContent context = case hContent of
        Text text -> ""
        Bullet text -> ""
        Todo{state, text}-> ""

instance EwwFmt [TextToken] () where
    ewwFmt tokens context = foldl (\acc t -> acc ++ ewwFmt t context) "" tokens

instance EwwFmt TextToken () where
    ewwFmt (Pure str) _ = str
    ewwFmt (Verbatim text) context =
        ""
    ewwFmt (Underline text) context =
        ""
    ewwFmt (Crossed text) context =
        ""
    ewwFmt (Bold text) context =
        ""
    ewwFmt (Italic text) context =
        ""
