module Heffal.Format.Json (jsonFmt, jsonFmtFile) where

import Data.Maybe
import Heffal.Format
import Heffal.Helper
import Heffal.Lexer (TextToken (..))
import Heffal.Parser

class JsonFmt a c where
  jsonFmt :: a -> c -> FmtConfig c -> String

instance JsonFmt File () where
  jsonFmt (File xs) context conf@FmtConfig {heading} =
    let f = fromMaybeShow jsonFmt heading context conf
     in "[" ++ joinStr f xs "," ++ "]"

instance JsonFmt Heading () where
  jsonFmt Heading {name, contents} context conf@FmtConfig {headingContents, textTokens} =
    let f = fromMaybeShow jsonFmt headingContents context conf
        fT = fromMaybe jsonToks (textTokens >>= \a -> Just $ show . a)
     in "{\"name\":" ++ fT name ++ ",\"contents\":" ++ f contents ++ "}"

instance JsonFmt [HeadingContent] () where
  jsonFmt contents context conf@FmtConfig {headingContent} =
    let f = fromMaybeShow jsonFmt headingContent context conf
     in "[" ++ joinStr f contents "," ++ "]"

instance JsonFmt HeadingContent () where
  jsonFmt hContent context conf@FmtConfig {textTokens} =
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

jsonFmtFile :: File -> () -> String
jsonFmtFile file context = jsonFmt file context fmtConfigDef

fromMaybeShow :: (a -> c -> FmtConfig c -> String) -> Maybe (a -> c -> FmtConfig c -> String) -> c -> FmtConfig c -> (a -> String)
fromMaybeShow dF f context conf = case f of
  Just f -> \h -> show $ f h context conf
  Nothing -> \h -> dF h context conf
