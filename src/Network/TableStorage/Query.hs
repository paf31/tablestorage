-- |
-- This module contains functions which help when unmarshalling query responses

module Network.TableStorage.Query (
  edmBinary, edmBoolean, edmDateTime, edmDouble,
  edmGuid, edmInt32, edmInt64, edmString
) where

import Data.Time ( UTCTime )
import Network.TableStorage.Types
    ( Entity(entityColumns),
      EntityColumn(EdmBinary, EdmBoolean, EdmDateTime, EdmDouble,
                   EdmGuid, EdmInt32, EdmInt64, EdmString) )

-- |
-- Find the value in a binary-valued column or return Nothing if no such column exists
edmBinary :: String -> Entity -> Maybe String
edmBinary key en = do
  col <- lookup key $ entityColumns en
  case col of
    EdmBinary s -> s
    _ -> Nothing

-- |
-- Find the value in a string-valued column or return Nothing if no such column exists
edmString :: String -> Entity -> Maybe String
edmString key en = do
  col <- lookup key $ entityColumns en
  case col of
    EdmString s -> s
    _ -> Nothing

-- |
-- Find the value in a boolean-valued column or return Nothing if no such column exists
edmBoolean :: String -> Entity -> Maybe Bool
edmBoolean key en = do
  col <- lookup key $ entityColumns en
  case col of
    EdmBoolean s -> s
    _ -> Nothing

-- |
-- Find the value in a date-valued column or return Nothing if no such column exists
edmDateTime :: String -> Entity -> Maybe UTCTime
edmDateTime key en = do
  col <- lookup key $ entityColumns en
  case col of
    EdmDateTime s -> s
    _ -> Nothing

-- |
-- Find the value in a double-valued column or return Nothing if no such column exists
edmDouble :: String -> Entity -> Maybe Double
edmDouble key en = do
  col <- lookup key $ entityColumns en
  case col of
    EdmDouble s -> s
    _ -> Nothing

-- |
-- Find the value in a Guid-valued column or return Nothing if no such column exists
edmGuid :: String -> Entity -> Maybe String
edmGuid key en = do
  col <- lookup key $ entityColumns en
  case col of
    EdmGuid s -> s
    _ -> Nothing

-- |
-- Find the value in an integer-valued column or return Nothing if no such column exists
edmInt32 :: String -> Entity -> Maybe Int
edmInt32 key en = do
  col <- lookup key $ entityColumns en
  case col of
    EdmInt32 s -> s
    _ -> Nothing

-- |
-- Find the value in an integer-valued column or return Nothing if no such column exists
edmInt64 :: String -> Entity -> Maybe Int
edmInt64 key en = do
  col <- lookup key $ entityColumns en
  case col of
    EdmInt64 s -> s
    _ -> Nothing