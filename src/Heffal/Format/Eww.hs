module Heffal.Format.Eww (ewwFmt, ewwFmtFile, ewwToks) where

import Data.Maybe
import Heffal.Format
import Heffal.Helper
import Heffal.Lexer (TextToken (..))
import Heffal.Parser
import Heffal.Config

class EwwFmt a where
  ewwFmt :: a -> Styles -> FmtConfig -> String

instance EwwFmt File where
  ewwFmt (File xs) styles conf@FmtConfig {heading} =
    let f h = fromMaybe ewwFmt heading h styles conf in joinStr f xs ""

instance EwwFmt Heading where
  ewwFmt Heading {name, contents} styles conf@FmtConfig {headingContents} =
    let f hC = fromMaybe ewwFmt headingContents hC styles conf in ""

instance EwwFmt [HeadingContent] where
  ewwFmt headings styles conf@FmtConfig {headingContent} =
    let f hC = fromMaybe ewwFmt headingContent hC styles conf in joinStr f headings ""

instance EwwFmt HeadingContent where
  ewwFmt hContent styles FmtConfig {textTokens} =
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

ewwFmtFile :: File -> Styles -> String
ewwFmtFile file styles = ewwFmt file styles fmtConfigDef
