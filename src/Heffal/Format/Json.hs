module Heffal.Format.Json (jsonFmt, jsonFmtFile, jsonToks) where

import qualified Data.Map as Map
import Data.Maybe
import Heffal.Format
import Heffal.Helper
import Heffal.Lexer (TextToken (..))
import Heffal.Parser
import Heffal.Config

class JsonFmt a where
  jsonFmt :: a -> Styles -> FmtConfig -> String

instance JsonFmt File where
  jsonFmt (File xs) styles conf@FmtConfig {heading} =
    let f h = fromMaybe jsonFmt heading h styles conf
     in "[" ++ joinStr f xs "," ++ "]"

instance JsonFmt Heading where
  jsonFmt Heading {name, contents} styles conf@FmtConfig {headingContents, textTokens} =
    let fT = fromMaybe jsonToks (textTokens >>= \f' -> Just $ show . f')
        f = fromMaybeShow jsonFmt headingContents styles conf
     in prettify $ Object [("name", Raw $ fT name), ("contents", Raw $ f contents)]

instance JsonFmt [HeadingContent] where
  jsonFmt contents styles conf@FmtConfig {headingContent} =
    let f = fromMaybeShow jsonFmt headingContent styles conf
     in "[" ++ joinStr f contents "," ++ "]"

instance JsonFmt HeadingContent where
  jsonFmt hContent Styles{bullet, todo_state, todo_state_conf = TodoStateConf{empty, brackets}} FmtConfig {textTokens} =
    let f = fromMaybe jsonToks (textTokens >>= (\t -> Just $ show . t))
     in case hContent of
          Text text -> buildStr "text" (f text) []
          Bullet text -> buildStr "bullet" (f text) [("style", String bullet)]
          Todo {state, text} ->
              let styled_state = (case Map.lookup state todo_state of
                                  _ | all (==' ') state -> empty
                                  Just s -> s
                                  _ -> state)
                  brackets_state = if brackets then "[" ++ styled_state ++ "]" else styled_state
              in buildStr "todo" (f text) [("state", String brackets_state)]
    where
      buildStr t contents extra = prettify . Object $ [("type", String t), ("contents", Raw contents)] ++ extra

textTok :: TextToken -> String
textTok (Pure str) = textTokBuild "string" str
textTok (Verbatim text) = textTokBuild "verbatim" $ jsonToks text
textTok (Underline text) = textTokBuild "underline" $ jsonToks text
textTok (Crossed text) = textTokBuild "crossed" $ jsonToks text
textTok (Bold text) = textTokBuild "bold" $ jsonToks text
textTok (Italic text) = textTokBuild "italic" $ jsonToks text

textTokBuild :: String -> String -> String
textTokBuild t contents = prettify $ Object [("type", String t), ("contents", Raw $ show contents)]

jsonToks :: [TextToken] -> String
jsonToks tokens = "[" ++ joinStr textTok tokens "," ++ "]"

jsonFmtFile :: File -> Styles -> String
jsonFmtFile file styles = jsonFmt file styles fmtConfigDef

fromMaybeShow :: (a -> Styles -> FmtConfig -> String) -> Maybe (a -> Styles -> FmtConfig -> String) -> Styles -> FmtConfig -> (a -> String)
fromMaybeShow dF f styles conf = case f of
  Just f' -> \h -> show $ f' h styles conf
  Nothing -> \h -> dF h styles conf

data Json = Object [(String, Json)]
          | Array [Json]
          | String String
          | Number Int
          | Bool Bool
          | Null
          | Raw String

prettify :: Json -> String
prettify json =
    case json of
        Object kvs -> let kvP (k, v) = show k ++ ":" ++ prettify v in "{" ++ joinStr kvP kvs "," ++ "}"
        Array jsons -> "[" ++ joinStr prettify jsons "," ++ "]"
        String string -> show string
        Number num -> show num
        Bool True -> "true"
        Bool False -> "false"
        Null -> "null"
        Raw raw -> raw