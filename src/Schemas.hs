{-# LANGUAGE TemplateHaskell              #-}
{-# LANGUAGE QuasiQuotes                  #-}
{-# LANGUAGE OverloadedStrings            #-}
{-# LANGUAGE DerivingStrategies           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving   #-}
{-# LANGUAGE StandaloneDeriving           #-}
{-# LANGUAGE UndecidableInstances         #-}
{-# LANGUAGE DataKinds                    #-}
{-# LANGUAGE FlexibleInstances            #-}
{-# LANGUAGE TypeFamilies                 #-}
{-# LANGUAGE GADTs                        #-}
{-# LANGUAGE MultiParamTypeClasses        #-}
{-# LANGUAGE TypeOperators                #-}

module Schemas where

import Database.Persist.TH

data Gender = Male | Female
  deriving (Show, Read, Eq)

derivePersistField "Gender"

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Country
  name String
  canWeSend Bool default=True
  UniqueCountryName name
  deriving Show
Client
  user  String
  firstName String
  lastName  String
  address   String
  country   CountryId
  age       Int Maybe sql=edad
  gender    Gender Maybe
  UniqueUser user
  UniqueClient firstName lastName address country
  deriving Show
Product
  name          String
  description   String
  price         Double
  stock         Int
  deriving Show
Purchase
  client        ClientId
  product       ProductId
  number        Int
  amount        Double
  deriving Show
|]




