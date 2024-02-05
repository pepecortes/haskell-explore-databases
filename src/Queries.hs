{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}

module Queries where
import Database.Persist
import Database.Persist.Sqlite ( SqlBackend )
import Control.Monad.IO.Unlift ( MonadUnliftIO )
import Control.Monad.Reader ( ReaderT )
import Control.Monad.Logger ( NoLoggingT )
import Conduit ( ResourceT )

-- use Esqueleto for SQL queries that are more complex than Persist functions
import qualified Database.Esqueleto as E

import Schemas

type SingleSelector m a = ReaderT SqlBackend (NoLoggingT (ResourceT m)) (Maybe (Entity a))
type ListSelector m a = ReaderT SqlBackend (NoLoggingT (ResourceT m)) [Entity a]

--type SqlPersistM = SqlPersistT (NoLoggingT (ResourceT IO))

getClientExample :: (MonadUnliftIO m) => SingleSelector m Client
getClientExample =  do
    maybeCountry <- getBy $ UniqueCountryName "Poland"
    case maybeCountry of
        Nothing -> pure Nothing
        Just (Entity idPoland _) -> getBy $ UniqueClient "Pepe" "Cortes" "Algunsitio" idPoland

getByUserExample :: (MonadUnliftIO m) => SingleSelector m Client
getByUserExample = getBy $ UniqueUser "donpepe"

getProductList :: (MonadUnliftIO m) => ListSelector m Product
getProductList = selectList [ProductPrice <. 500] []

getProductListWithSQL :: (MonadUnliftIO m) => ListSelector m Product
getProductListWithSQL = do
    E.select $
        E.from $ \product -> do
        pure product

getPurchases :: (MonadUnliftIO m) => ListSelector m Purchase
getPurchases = selectList [] []