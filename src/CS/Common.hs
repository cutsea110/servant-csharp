{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module CS.Common where

import Data.Monoid ((<>))
import Data.Proxy
import Data.Text
import Data.Time (UTCTime)
import Data.Time.Calendar (Day)
import Data.Typeable
import Servant.Foreign

data CSharp

instance HasForeignType CSharp Text a => HasForeignType CSharp Text [a] where
    typeFor lang ftype (Proxy :: Proxy [t]) = "List<" <> typeFor lang ftype (Proxy :: Proxy t) <> ">"

instance HasForeignType CSharp Text a => HasForeignType CSharp Text (Maybe a) where
    typeFor lang ftype (Proxy :: Proxy (Maybe t)) = "Nullable<" <> typeFor lang ftype (Proxy :: Proxy t) <> ">"

instance {-# OVERLAPPING #-} HasForeignType CSharp Text () where
    typeFor _ _ _ = "void"

instance {-# OVERLAPPING #-} HasForeignType CSharp Text Int where
    typeFor _ _ _ = "int"
instance {-# OVERLAPPING #-} HasForeignType CSharp Text (Maybe Int) where
    typeFor _ _ _ = "int?"

instance {-# OVERLAPPING #-} HasForeignType CSharp Text Text where
    typeFor _ _ _ = "string"
instance {-# OVERLAPPING #-} HasForeignType CSharp Text (Maybe Text) where
    typeFor _ _ _ = "string"

instance {-# OVERLAPPING #-} HasForeignType CSharp Text String where
    typeFor _ _ _ = "string"
instance {-# OVERLAPPING #-} HasForeignType CSharp Text (Maybe String) where
    typeFor _ _ _ = "string"

instance {-# OVERLAPPING #-} HasForeignType CSharp Text Day where
    typeFor _ _ _ = "DateTime"
instance {-# OVERLAPPING #-} HasForeignType CSharp Text (Maybe Day) where
    typeFor _ _ _ = "DateTime?"

instance {-# OVERLAPPING #-} HasForeignType CSharp Text UTCTime where
    typeFor _ _ _ = "DateTime"
instance {-# OVERLAPPING #-} HasForeignType CSharp Text (Maybe UTCTime) where
    typeFor _ _ _ = "DateTime?"

instance {-# OVERLAPS #-} Typeable t => HasForeignType CSharp Text t where
    typeFor lang ftype p = pack $ show $ typeRep p

getEndpoints :: (HasForeign CSharp Text api,
                 GenerateList Text (Foreign Text api)) =>
                Proxy api -> [Req Text]
getEndpoints = listFromAPI (Proxy :: Proxy CSharp) (Proxy :: Proxy Text)

