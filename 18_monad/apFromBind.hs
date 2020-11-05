module ApFromBind where

ap :: Monad m => m (a -> b) -> m a -> m b
ap f x= do
    f' <- f
    x' <- x
    return (f' x')

ap' :: Monad m => m (a -> b) -> m a -> m b
ap' f x = f >>=
    \f' -> x >>= 
    \x' -> return (f' x')