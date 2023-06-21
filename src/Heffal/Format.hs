module Heffal.Format (FmtConfig (..), fmtConfigDef) where

import Heffal.Lexer
import Heffal.Parser
import Heffal.Config

type FmtConfigToStr a = Maybe (a -> Styles -> FmtConfig -> String)

data FmtConfig = FmtConfig
  { heading :: FmtConfigToStr Heading,
    headingContents :: FmtConfigToStr [HeadingContent],
    headingContent :: FmtConfigToStr HeadingContent,
    textTokens :: Maybe ([TextToken] -> String)
  }

fmtConfigDef :: FmtConfig
fmtConfigDef =
  FmtConfig
    { heading = Nothing,
      headingContents = Nothing,
      headingContent = Nothing,
      textTokens = Nothing
    }
