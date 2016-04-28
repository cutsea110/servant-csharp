{-# LANGUAGE FlexibleInstances #-}
module CS.JsonDotNet.Base where

import Control.Monad.Trans
import Control.Monad.Identity
import Data.Proxy
import Data.Swagger hiding (namespace)
import Servant.Swagger

type Swag = SwagT Identity

runSwagger :: Swag a -> Swagger -> a
runSwagger f = runIdentity . runSwagT f

mkSwag :: Monad m => (Swagger -> a) -> SwagT m a
mkSwag f = SwagT (return . f)

generateFrom :: HasSwagger api => Swag a -> Proxy api -> a
f `generateFrom` api = runSwagger f (toSwagger api)

newtype SwagT m a = SwagT { runSwagT :: Swagger -> m a }
instance Monad m => Functor (SwagT m) where
    fmap f x = SwagT $ \sw -> return . f =<< runSwagT x sw
instance Monad m => Applicative (SwagT m) where
    pure x = SwagT $ \sw -> return x
    f <*> g = SwagT $ \sw -> do
                f' <- runSwagT f sw
                g' <- runSwagT g sw
                return (f' g')
instance Monad m => Monad (SwagT m) where
    f >>= k = SwagT $ \sw ->
              runSwagT f sw >>= \f' ->
              runSwagT (k f') sw

instance Monad m => Monoid (SwagT m [a]) where
    mempty = SwagT $ \sw -> return mempty
    x `mappend` y = SwagT $ \sw -> do
                      x' <- runSwagT x sw
                      y' <- runSwagT y sw
                      return $ x' `mappend` y'

instance MonadTrans SwagT where
    lift m = SwagT $ \sw -> m

instance MonadIO m => MonadIO (SwagT m) where
    liftIO x = SwagT $ \sw -> liftIO x
