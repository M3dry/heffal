module Heffal.Format.Eww (ewwFormatter, ewwFmtFile, ewwToks, Eww(..)) where

import Data.Maybe
import Heffal.Config
import Heffal.Format
import Heffal.Helper
import Heffal.Lexer (TextToken (..))
import Heffal.Parser

data Eww
    = Box [(String, Eww)] [Eww]
    | Label [(String, Eww)]
    | ETrue
    | EFalse
    | String String
    | Raw String
    deriving Show

instance Stringify Eww where
    stringify :: Eww -> String
    stringify eww =
        case eww of
            Box props body -> "(box " ++ joinStr (\(k, v) -> ":" ++ k ++ " " ++ show v) props " " ++ " " ++ joinStr stringify body " " ++ ")"
            Label props -> "(label " ++ joinStr (\(k, v) -> ":" ++ k ++ " " ++ show v) props " " ++ ")"
            String string -> show string
            ETrue -> "true"
            EFalse -> "false"
            Raw raw -> raw

class EwwFmt a where
    ewwFormatter :: Formatter a Eww c d e

instance EwwFmt File where
    ewwFormatter = Formatter
        { create = \(File xs) styles conf@FmtConfig{heading} ->
            maybeEww
                heading
                (\f -> let f' h = createFmt f h styles conf in Box [] [])
                (let f' h = create ewwFormatter h styles conf in Box [] [])
        , format = stringify
        }

instance EwwFmt Heading where
    ewwFormatter = Formatter
        { create = \Heading{name, contents} styles conf@FmtConfig{headingContents} ->
            maybeEww
                headingContents
                (\f -> let f' h = createFmt f h styles conf in Box [] [])
                (let f' h = createFmt ewwFormatter h styles conf in Box [] [])
        , format = stringify
        }

instance EwwFmt [HeadingContent] where
    ewwFormatter = Formatter
        { create = \headings styles conf@FmtConfig{headingContent} ->
            maybeEww
                headingContent
                (\f -> let f' h = createFmt f h styles conf in Box [] [])
                (let f' h = createFmt ewwFormatter h styles conf in Box [] [])
        , format = stringify
        }

instance EwwFmt HeadingContent where
    ewwFormatter = Formatter
        { create = \hContent styles FmtConfig{textTokens} ->
            let f toks =
                    maybeEww
                        textTokens
                        (\f -> String $ f toks)
                        (ewwToks toks)
            in undefined $ case hContent of
                Text text -> f text
                Bullet text -> f text
                Todo{state, text} -> f text
        , format = stringify
        }

textTok :: TextToken -> Eww
textTok token =
        case token of
            Pure str -> Label [("halign", String "start"), ("text", String str)]
            Verbatim text -> box "color: #c3e88d;" text
            Underline text -> box "text-decoration: underline;" text
            Crossed text -> box "text-decoration: line-through;" text
            Bold text -> box "font-weight: bold;" text
            Italic text -> box "font-style: italic;" text
  where
    box style = Box [("style", String style)] . textToks



textToks :: [TextToken] -> [Eww]
textToks = map textTok

ewwToks :: [TextToken] -> Eww
ewwToks tokens = Box [("space-evenly", EFalse)] $ map textTok tokens

ewwFmtFile :: File -> Styles -> String
ewwFmtFile file styles = createFmt ewwFormatter file styles fmtConfigDef

fromMaybeFormatter :: Formatter a b c d e -> Maybe (Formatter a f c d e) -> Styles -> FmtConfig c d e -> (a -> String)
fromMaybeFormatter dFormatter f styles conf =
    case f of
        Just f' -> \h -> show $ createFmt f' h styles conf
        Nothing -> \h -> createFmt dFormatter h styles conf

maybeEww :: Maybe a -> (a -> Eww) -> Eww -> Eww
maybeEww formatter onJust onNothing =
    case formatter of
        Just f -> onJust f
        _ -> onNothing
