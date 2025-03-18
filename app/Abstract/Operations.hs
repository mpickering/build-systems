{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
module Abstract.Operations where

data Operations k r m = Operations
        { fetch :: forall a . k a -> m (r a)
        , fetches :: forall a . [k a] -> m [r a] }

type OperationsIO k r = Operations k r IO