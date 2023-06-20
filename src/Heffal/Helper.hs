module Heffal.Helper where

subStr :: String -> Int -> Int -> String
subStr str start end = take (end - start + 1) $ drop start str

joinStr :: (a -> String) -> [a] -> String -> String
joinStr _ [] _ = ""
joinStr f [h] _ = f h
joinStr f (h:as) str = f h ++ foldl (\acc a -> acc ++ str ++ f a) "" as

justIs :: Maybe a -> (a -> Bool) -> Bool
justIs just f = maybe False f just
