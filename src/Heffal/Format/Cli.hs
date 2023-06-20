module Heffal.Format.Cli (cliFmt, cliFmtFile, cliToks) where

import Data.Maybe
import Heffal.Format
import Heffal.Helper
import Heffal.Lexer (TextToken (..))
import Heffal.Parser

class CliFmt a c where
    cliFmt :: a -> c -> FmtConfig c -> String

instance CliFmt File () where
    cliFmt (File xs) context conf@FmtConfig{heading} =
        let f h = fromMaybe cliFmt heading h context conf
         in joinStr f xs "\n\n"

instance CliFmt Heading () where
    cliFmt Heading{name, contents} context conf@FmtConfig{headingContents, textTokens} =
        let f hC = fromMaybe cliFmt headingContents hC context conf
            fT = fromMaybe cliToks textTokens
         in "Heading "
                ++ fT name
                ++ "\n"
                ++ f contents

instance CliFmt [HeadingContent] () where
    cliFmt contents context conf@FmtConfig{headingContent} =
        let f hC = fromMaybe cliFmt headingContent hC context conf
         in joinStr f contents "\n"

instance CliFmt HeadingContent () where
    cliFmt hContent context conf@FmtConfig{textTokens} =
        let f = fromMaybe cliToks textTokens
         in case hContent of
                Text text -> "  " ++ f text
                Bullet text -> "  - " ++ f text
                Todo{state, text} -> "  [" ++ state ++ "] " ++ f text

textTok :: TextToken -> String
textTok (Pure str) = str
textTok (Verbatim text) =
    textFmt '`' text
textTok (Underline text) =
    textFmt '_' text
textTok (Crossed text) =
    textFmt '-' text
textTok (Bold text) =
    textFmt '*' text
textTok (Italic text) =
    textFmt '/' text

cliToks :: [TextToken] -> String
cliToks tokens = joinStr textTok tokens ""

textFmt :: Char -> [TextToken] -> String
textFmt ch text = ch : cliToks text ++ [ch]

cliFmtFile :: File -> () -> String
cliFmtFile file context = cliFmt file context fmtConfigDef
