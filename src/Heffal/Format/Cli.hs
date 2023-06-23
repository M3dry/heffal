module Heffal.Format.Cli (cliFormatter, cliFmtFile, cliToks) where

import Data.Maybe
import Heffal.Format
import Heffal.Helper
import Heffal.Lexer (TextToken (..))
import Heffal.Parser
import Heffal.Config

class CliFmt a where
    cliFormatter :: Formatter a String c d e

instance CliFmt File where
    cliFormatter = Formatter
        { create = \(File xs) styles conf@FmtConfig{heading} ->
            let f = fromMaybeFormatter cliFormatter heading styles conf
             in joinStr f xs "\n\n"
        , format = id
        }

instance CliFmt Heading where
    cliFormatter = Formatter
        { create = \Heading{name, contents} styles conf@FmtConfig{headingContents, textTokens} ->
            let f = fromMaybeFormatter cliFormatter headingContents styles conf
                fT = fromMaybe cliToks textTokens
             in "Heading "
                    ++ fT name
                    ++ "\n"
                    ++ f contents
        , format = id
        }

instance CliFmt [HeadingContent] where
    cliFormatter = Formatter
        { create = \contents styles conf@FmtConfig{headingContent} ->
            let f = fromMaybeFormatter cliFormatter headingContent styles conf
             in joinStr f contents "\n"
        , format = id
        }

instance CliFmt HeadingContent where
    cliFormatter = Formatter
        { create = \hContent Styles{bullet, todo_state, todo_state_conf = TodoStateConf{empty, brackets}} FmtConfig{textTokens} ->
            let f = fromMaybe cliToks textTokens
             in case hContent of
                    Text text -> indent ++ f text
                    Bullet text -> indent ++ bullet ++ " " ++ f text
                    Todo{state, text} ->
                         let styled_state = (case state `lookup` todo_state of
                                _ | all (==' ') state -> empty
                                Just s -> s
                                _ -> state)
                             brackets_state = if brackets then "[" ++ styled_state ++ "]" else styled_state
                        in
                         indent ++ brackets_state ++ " " ++ f text
        , format = id
        }
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
cliFmtFile file styles = create cliFormatter file styles fmtConfigDef


fromMaybeFormatter :: Formatter a b c d e -> Maybe (Formatter a f c d e) -> Styles -> FmtConfig c d e -> (a -> String)
fromMaybeFormatter dFormatter f styles conf =
    case f of
        Just f' -> \h -> createFmt f' h styles conf
        Nothing -> \h -> createFmt dFormatter h styles conf
