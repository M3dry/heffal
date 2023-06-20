module Heffal.Format.Eww (ewwFmt, ewwFmtFile, ewwToks) where

import Data.Maybe
import Heffal.Format
import Heffal.Helper
import Heffal.Lexer (TextToken (..))
import Heffal.Parser

class EwwFmt a c where
  ewwFmt :: a -> c -> FmtConfig c -> String

instance EwwFmt File () where
  ewwFmt (File xs) context conf@FmtConfig {heading} =
    let f h = fromMaybe ewwFmt heading h context conf in joinStr f xs ""

instance EwwFmt Heading () where
  ewwFmt Heading {name, contents} context conf@FmtConfig {headingContents} =
    let f hC = fromMaybe ewwFmt headingContents hC context conf in ""

instance EwwFmt [HeadingContent] () where
  ewwFmt headings context conf@FmtConfig {headingContent} =
    let f hC = fromMaybe ewwFmt headingContent hC context conf in joinStr f headings ""

instance EwwFmt HeadingContent () where
  ewwFmt hContent context conf@FmtConfig {textTokens} =
    let f = fromMaybe ewwToks textTokens
     in case hContent of
          Text text -> ""
          Bullet text -> ""
          Todo {state, text} -> ""

textTok :: TextToken -> String
textTok (Pure str) = "(label :halign \"start\" :text \"" ++ str ++ "\")"
textTok (Verbatim text) =
  box "color: #c3e88d;" $ textToks text
textTok (Underline text) =
  box "text-decoration: underline;" $ textToks text
textTok (Crossed text) =
  box "text-decoration: line-through;" $ textToks text
textTok (Bold text) =
  box "font-weight: bold;" $ textToks text
textTok (Italic text) =
  box "font-style: italic;" $ textToks text

textToks :: [TextToken] -> String
textToks tokens = joinStr textTok tokens ""

ewwToks :: [TextToken] -> String
ewwToks tokens = "(box :space-evenly false " ++ joinStr textTok tokens "" ++ ")"

box :: String -> String -> String
box style contents =
  "(box :style \"" ++ style ++ "\" :halign \"start\" " ++ contents ++ ")"

ewwFmtFile :: File -> () -> String
ewwFmtFile file context = ewwFmt file context fmtConfigDef
