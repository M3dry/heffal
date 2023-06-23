module Heffal.Format.Json (jsonFormatter, jsonFmtFile, jsonToks, Json (..)) where

import Data.Maybe
import Heffal.Config
import Heffal.Format
import Heffal.Helper
import Heffal.Lexer (TextToken (..))
import Heffal.Parser

data Json
    = Object [(String, Json)]
    | Array [Json]
    | String String
    | Number Int
    | Bool Bool
    | Null
    | Raw String
    deriving (Show)

instance Stringify Json where
    stringify json =
        case json of
            Object kvs -> let kvP (k, v) = show k ++ ":" ++ stringify v in "{" ++ joinStr kvP kvs "," ++ "}"
            Array jsons -> "[" ++ joinStr stringify jsons "," ++ "]"
            String string -> show string
            Number num -> show num
            Bool True -> "true"
            Bool False -> "false"
            Null -> "null"
            Raw raw -> raw

class JsonFmt a where
    jsonFormatter :: Formatter a Json c d e

instance JsonFmt File where
    jsonFormatter =
        Formatter
            { create = \(File xs) styles conf@FmtConfig{heading} ->
                maybeJson
                    heading
                    (\f -> let f' h = createFmt f h styles conf in Array $ map (String . f') xs)
                    (let f' h = create jsonFormatter h styles conf in Array $ map f' xs)
            , format = stringify
            }

instance JsonFmt Heading where
    jsonFormatter =
        Formatter
            { create = \Heading{name, contents} styles conf@FmtConfig{headingContents, textTokens} ->
                let toks = maybeJson textTokens (\f -> String $ f name) (jsonToks name)
                    contents' =
                        maybeJson
                            headingContents
                            (\f -> let f' h = createFmt f h styles conf in String $ f' contents)
                            (let f' h = create jsonFormatter h styles conf in f' contents)
                 in Object [("name", toks), ("contents", contents')]
            , format = stringify
            }

instance JsonFmt [HeadingContent] where
    jsonFormatter =
        Formatter
            { create = \contents styles conf@FmtConfig{headingContent} ->
                maybeJson
                    headingContent
                    (\f -> let f' h = createFmt f h styles conf in Array $ map (String . f') contents)
                    (let f' h = create jsonFormatter h styles conf in Array $ map f' contents)
            , format = stringify
            }

instance JsonFmt HeadingContent where
    jsonFormatter =
        Formatter
            { create = \hContent Styles{bullet, todo_state, todo_state_conf = TodoStateConf{empty, brackets}} FmtConfig{textTokens} ->
                let f toks =
                        maybeJson
                            textTokens
                            (\f -> String $ f toks)
                            (jsonToks toks)
                 in case hContent of
                        Text text -> buildObj "text" (f text) []
                        Bullet text -> buildObj "bullet" (f text) [("style", String bullet)]
                        Todo{state, text} ->
                            let styled_state =
                                    ( case state `lookup` todo_state of
                                        _ | all (== ' ') state -> empty
                                        Just s -> s
                                        _ -> state
                                    )
                                brackets_state = if brackets then "[" ++ styled_state ++ "]" else styled_state
                             in buildObj "todo" (f text) [("state", String brackets_state)]
            , format = stringify
            }
      where
        buildObj t contents extra = Object $ [("type", String t), ("contents", contents)] ++ extra

textTok :: TextToken -> Json
textTok tok =
    case tok of
        Pure str -> textTokBuild "string" $ String str
        Verbatim text -> textTokBuild "verbatim" $ jsonToks text
        Underline text -> textTokBuild "underline" $ jsonToks text
        Crossed text -> textTokBuild "crossed" $ jsonToks text
        Bold text -> textTokBuild "bold" $ jsonToks text
        Italic text -> textTokBuild "italic" $ jsonToks text
  where
    textTokBuild :: String -> Json -> Json
    textTokBuild t contents = Object [("type", String t), ("contents", contents)]

jsonToks :: [TextToken] -> Json
jsonToks tokens = Array $ map textTok tokens

jsonFmtFile :: File -> Styles -> String
jsonFmtFile file styles = createFmt jsonFormatter file styles fmtConfigDef

maybeJson :: Maybe a -> (a -> Json) -> Json -> Json
maybeJson formatter onJust onNothing =
    case formatter of
        Just f -> onJust f
        _ -> onNothing
