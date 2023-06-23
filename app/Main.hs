{-# LANGUAGE QuasiQuotes #-}

module Main where

import Data.Maybe
import Heffal.Config
import Heffal.Format
import Heffal.Format.Cli
import Heffal.Format.Eww
import Heffal.Format.Json
import Heffal.Lexer
import Heffal.Parser

main :: IO ()
main = do
    let ast =
            [heffal|# heading
[ ] todo text
- bullet "text"
normal text

# another heading
- another bullet *text*
|]
    print $
        create jsonFormatter
            ast
            stylesDef
                { bullet = "+"
                , todo_state_conf = TodoStateConf{empty = "/*\\", brackets = False}
                }
            fmtConfigDef{ textTokens = Just cliToks }
