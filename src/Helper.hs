module Helper where

subStr :: String -> Int -> Int -> String
subStr str start end = take (end - start) $ drop start str

justIs :: Eq a => Maybe a -> (a -> Bool) -> Bool
justIs just f = maybe False f just