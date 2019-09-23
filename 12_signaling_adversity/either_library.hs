module EitherLib where


lefts' :: [Either a b] -> [a]
lefts' eithers =
    foldr appendIfHasLeft [] eithers
    where
        appendIfHasLeft either acc =
            case left either of
                Nothing -> acc
                Just x -> [x] ++ acc

rights' :: [Either a b] -> [b]
rights' eithers = lefts' $ map flipEither eithers

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' eithers = (lefts' eithers, rights' eithers)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Left x) = Nothing
eitherMaybe' f (Right x) = Just $ f x

eitherMaybe'' :: (a -> c) -> Either a b -> Maybe c
eitherMaybe'' f either = eitherMaybe' f (flipEither either)

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f g (Left x) = f x
either' f g (Right x) = g x

left :: Either a b -> Maybe a
left (Right x) = Nothing
left (Left x) = Just x

right :: Either a b -> Maybe b
right e = left $ flipEither e

flipEither :: Either a b -> Either b a
flipEither (Left x) = Right x
flipEither (Right x) = Left x