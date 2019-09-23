module MaybeLib where


isJust :: Maybe a -> Bool
isJust (Just x) = True
isJust _ = False

isNothing :: Maybe a -> Bool
isNothing m = not $ isJust m

fromMaybe :: a -> Maybe a -> a
fromMaybe x (Just y) = y
fromMaybe x Nothing = x

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee x f Nothing = x
mayybee x f (Just y) = f y

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = Just x

maybeToList :: Maybe a -> [a]
maybeToList (Just x) = [x]
maybeToList Nothing = []

catMaybes :: [Maybe a] -> [a]
catMaybes ms =
    map (\x -> fromMaybe undefined x) justs
    where
        justs = filter isJust ms

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe ms =
    if hasNothing
        then Nothing
        else Just $ map (\x -> fromMaybe undefined x) ms
    where
        hasNothing = any isNothing ms