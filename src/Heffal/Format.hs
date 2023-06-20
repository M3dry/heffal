module Heffal.Format (FmtConfig (..), fmtConfigDef) where

import Heffal.Lexer
import Heffal.Parser

type FmtConfigToStr a c = Maybe (a -> c -> FmtConfig c -> String)

data FmtConfig c = FmtConfig
  { heading :: FmtConfigToStr Heading c,
    headingContents :: FmtConfigToStr [HeadingContent] c,
    headingContent :: FmtConfigToStr HeadingContent c,
    textTokens :: Maybe ([TextToken] -> String)
  }

fmtConfigDef :: FmtConfig c
fmtConfigDef =
  FmtConfig
    { heading = Nothing,
      headingContents = Nothing,
      headingContent = Nothing,
      textTokens = Nothing
    }
