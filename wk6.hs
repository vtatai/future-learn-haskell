import Control.Monad

baz :: [a] -> Maybe a

{-baz xs = do  -}
    {-a <- myTail xs-}
    {-b <- myTail a-}
    {-c <- myHead b-}
    {-return c-}

myTail :: [a] -> Maybe [a]
myTail [] = Nothing
myTail (a:as) = Just as

myHead :: [a] -> Maybe a
myHead [] = Nothing
myHead (a:as) = Just a

baz xs = myTail xs >>= (\a -> myTail a >>= (\b -> myHead b >>= (\c -> return c)))


data Maybe' a = Nothing' | Just' a 
    deriving Show

instance Functor Maybe' where
  fmap = liftM

instance Applicative Maybe' where
  pure  = return
  (<*>) = ap

instance Monad Maybe' where
    return = Just'
    Nothing' >>= f = Nothing'
    (Just' x) >>= f = f x
    fail _ = Nothing'


