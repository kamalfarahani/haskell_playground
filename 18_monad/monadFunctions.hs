module MonadFunctions where


j :: Monad m => m (m a) -> m a
j mma = mma >>= id

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f ma mb = 
    ma >>=
    \a -> mb >>=
    \b -> return (f a b)

a :: Monad m => m a -> (m (a -> b)) -> m b
a ma mf = 
    mf >>=
    \f -> ma >>=
    \a -> return (f a)

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] f = return []
meh (x:xs) f =
    f x >>=
    \b -> (meh xs f) >>=
    \bs -> return ([b] ++ bs)

flipType :: Monad m => [m a] -> m [a]
flipType mas = meh mas id