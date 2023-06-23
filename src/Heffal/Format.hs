module Heffal.Format (Stringify(..), FmtConfig (..), fmtConfigDef, Formatter(..), formatter, createFmt, createFmtMaybe) where

import Heffal.Lexer
import Heffal.Parser
import Heffal.Config

class Stringify a where
    stringify :: a -> String

data Formatter a b c d e = Formatter
    { create :: a -> Styles -> FmtConfig c d e -> b
    , format :: b -> String
    }

formatter :: (a -> Styles -> FmtConfig c d e -> String) -> Formatter a String c d e
formatter fmt = Formatter fmt id

createFmt :: Formatter a b c d e -> a -> Styles -> FmtConfig c d e -> String
createFmt (Formatter create format) c s f = format $ create c s f

createFmtMaybe :: Maybe (Formatter a b c d e) -> Maybe (a -> Styles -> FmtConfig c d e -> String)
createFmtMaybe formatter = formatter >>= \f -> Just $ createFmt f

data FmtConfig a b c = FmtConfig
  { heading :: Maybe (Formatter Heading a a b c),
    headingContents :: Maybe (Formatter [HeadingContent] b a b c),
    headingContent :: Maybe (Formatter HeadingContent c a b c),
    textTokens :: Maybe ([TextToken] -> String)
  }

fmtConfigDef :: FmtConfig a b c
fmtConfigDef =
  FmtConfig
    { heading = Nothing,
      headingContents = Nothing,
      headingContent = Nothing,
      textTokens = Nothing
    }
