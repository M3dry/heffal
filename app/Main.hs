module Main where

import Data.Maybe
import Heffal.Format
import Heffal.Format.Cli
import Heffal.Format.Eww
import Heffal.Format.Json
import Heffal.Lexer
import Heffal.Parser

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
        >>= ( \(File xs) ->
                Just $
                  jsonFmt
                    (filter isTodo $ concatMap contents xs)
                    ()
                    fmtConfigDef {textTokens = Just ewwToks}
            )
  putStrLn ":END_TODO_ONLY"
