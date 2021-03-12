-- Safe functions

safeHead :: [x] -> Maybe x
safemyHead (x:_) = Just x
safemyHead [] = Nothing

safeTail :: [x] -> Maybe [a]
safeTail (x:xs) = Just xs
safeTail [] = Nothing

safeLast :: [x] -> Maybe a
safeLast xs = Just (last xs)
safeLast [] = Nothing

safeInit :: [x] -> Maybe [x]
safeInit xs = Just (init xs)
safeInit [] = Nothing


