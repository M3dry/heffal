module Main where

import qualified Data.Map as Map
import Data.Maybe
import Heffal.Format
import Heffal.Format.Cli
import Heffal.Format.Eww
import Heffal.Format.Json
import Heffal.Lexer
import Heffal.Parser
import Heffal.Config

main :: IO ()
main = do
  let tokens = strToTokens "# heading\n- this is a bullet\n\nAnd this is text\n[ ] this is a todo\n\n# nice `heading` *very bold move*"
  let ast = tokensToAST tokens
  -- print tokens
  -- print ast
  putStrLn "START_TODO_ONLY:"
  putStr $
    fromMaybe "" $
      ast
        >>= ( \xs ->
                Just $
                  cliFmt
                    xs
                    stylesDef{ bullet = "+", todo_state_conf = TodoStateConf{ empty = "/*\\", brackets = False } }
                    fmtConfigDef
            )
  putStrLn ":END_TODO_ONLY"
