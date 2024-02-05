{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}


import           Database.Persist.Sqlite
import           Control.Monad.IO.Unlift

import Schemas
import Queries

main :: IO ()
main = runSqlite "example.db" $ do
    --runMigration migrateAll
    --selection <- getClientExample
    --selection <- getByUserExample
    --selection <- getProductList
    selection <- getProductListWithSQL
    --selection <- getPurchases
    liftIO $ print selection