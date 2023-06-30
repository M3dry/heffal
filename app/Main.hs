module Main where

import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Maybe

import Args

main :: IO ()
main = main' =<< args

main' :: Args -> IO ()
main' = print
