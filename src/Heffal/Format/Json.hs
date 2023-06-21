module Heffal.Format.Json (jsonFmt, jsonFmtFile, jsonToks) where

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
    let f = fromMaybeShow jsonFmt heading styles conf
     in "[" ++ joinStr f xs "," ++ "]"

instance JsonFmt Heading where
  jsonFmt Heading {name, contents} styles conf@FmtConfig {headingContents, textTokens} =
    let f = fromMaybeShow jsonFmt headingContents styles conf
        fT = fromMaybe jsonToks (textTokens >>= \a -> Just $ show . a)
     in "{\"name\":" ++ fT name ++ ",\"contents\":" ++ f contents ++ "}"

instance JsonFmt [HeadingContent] where
  jsonFmt contents styles conf@FmtConfig {headingContent} =
    let f = fromMaybeShow jsonFmt headingContent styles conf
     in "[" ++ joinStr f contents "," ++ "]"

instance JsonFmt HeadingContent where
  jsonFmt hContent styles FmtConfig {textTokens} =
    let f = fromMaybe jsonToks (textTokens >>= \a -> Just $ show . a)
     in case hContent of
          Text text -> "{\"type\":\"text\",\"contents\":" ++ f text ++ "}"
          Bullet text -> "{\"type\":\"bullet\",\"contents\":" ++ f text ++ "}"
          Todo {state, text} -> "{\"type\":\"todo\",\"state\":" ++ show state ++ ",\"contents\":" ++ f text ++ "}"

textTok :: TextToken -> String
textTok (Pure str) = "{\"type\":\"string\",\"contents\":" ++ show str ++ "}"
textTok (Verbatim text) = "{\"type\":\"verbatim\",\"contents\":" ++ jsonToks text ++ "}"
textTok (Underline text) = "{\"type\":\"underline\",\"contents\":" ++ jsonToks text ++ "}"
textTok (Crossed text) = "{\"type\":\"crossed\",\"contents\":" ++ jsonToks text ++ "}"
textTok (Bold text) = "{\"type\":\"bold\",\"contents\":" ++ jsonToks text ++ "}"
textTok (Italic text) = "{\"type\":\"italic\",\"contents\":" ++ jsonToks text ++ "}"

jsonToks :: [TextToken] -> String
jsonToks tokens = "[" ++ joinStr textTok tokens "," ++ "]"

jsonFmtFile :: File -> Styles -> String
jsonFmtFile file styles = jsonFmt file styles fmtConfigDef

fromMaybeShow :: (a -> Styles -> FmtConfig -> String) -> Maybe (a -> Styles -> FmtConfig -> String) -> Styles -> FmtConfig -> (a -> String)
fromMaybeShow dF f styles conf = case f of
  Just f' -> \h -> show $ f' h styles conf
  Nothing -> \h -> dF h styles conf
