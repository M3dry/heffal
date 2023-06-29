-- {-# LANGUAGE QuasiQuotes #-}

module Main where

-- import Heffal.Config
-- import Heffal.Format
-- import Heffal.Format.Cli
-- import Heffal.Format.Eww
-- import Heffal.Format.Json
-- import Heffal.Lexer
-- import Heffal.Parser

import Args
import Args.Doc (OptionValue (OptionValue))
import Args.Parser
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Maybe

main :: IO ()
main = do
    print $ runArgs argsP $ SArgs ["-t", "show"]
