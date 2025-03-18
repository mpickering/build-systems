{-# LANGUAGE RankNTypes #-}
module Abstract.Operations where

data Operations k m = Operations
        { fetch :: forall a . k a -> m a
        , fetches :: forall a . [k a] -> m [a] }