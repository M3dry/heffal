module Heffal.Helper where

subStr :: String -> Int -> Int -> String
subStr str start end = take (end - start + 1) $ drop start str

justIs :: Maybe a -> (a -> Bool) -> Bool
justIs just f = maybe False f just
