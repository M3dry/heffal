module Heffal.Format.Cli (cliFmt, cliFmtFile, cliToks) where

import Data.Maybe
import qualified Data.Map.Strict as Map
import Heffal.Format
import Heffal.Helper
import Heffal.Lexer (TextToken (..))
import Heffal.Parser
import Heffal.Config

class CliFmt a where
    cliFmt :: a -> Styles -> FmtConfig -> String

instance CliFmt File where
    cliFmt (File xs) styles conf@FmtConfig{heading} =
        let f h = fromMaybe cliFmt heading h styles conf
         in joinStr f xs "\n\n"

instance CliFmt Heading where
    cliFmt Heading{name, contents} styles conf@FmtConfig{headingContents, textTokens} =
        let f hC = fromMaybe cliFmt headingContents hC styles conf
            fT = fromMaybe cliToks textTokens
         in "Heading "
                ++ fT name
                ++ "\n"
                ++ f contents

instance CliFmt [HeadingContent] where
    cliFmt contents styles conf@FmtConfig{headingContent} =
        let f hC = fromMaybe cliFmt headingContent hC styles conf
         in joinStr f contents "\n"

instance CliFmt HeadingContent where
    cliFmt hContent styles FmtConfig{textTokens} =
        let f = fromMaybe cliToks textTokens
         in case hContent of
                Text text -> indent ++ f text
                Bullet text -> indent ++ bullet styles ++ " " ++ f text
                Todo{state, text} ->
                     let styled_state = (case Map.lookup state $ todo_state styles of
                            _ | all (==' ') state -> empty $ todo_state_conf styles
                            Just s -> s
                            _ -> state)
                         brackets_state = if brackets $ todo_state_conf styles
                            then "[" ++ styled_state ++ "]"
                            else styled_state

                    in
                     indent ++ brackets_state ++ " " ++ f text
      where
        indent = "  "

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

cliFmtFile :: File -> Styles -> String
cliFmtFile file styles = cliFmt file styles fmtConfigDef
