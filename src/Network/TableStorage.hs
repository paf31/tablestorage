-- | A simple wrapper for the Azure Table Storage REST API
--
-- This module exists simply to re-export the following:
--
-- * "Network.TableStorage.Types"
--
-- * "Network.TableStorage.API"
--
-- * "Network.TableStorage.Development"

module Network.TableStorage (module TableStorage) where

import Network.TableStorage.Types as TableStorage
import Network.TableStorage.API as TableStorage
import Network.TableStorage.Development as TableStorage
import Network.TableStorage.Query as TableStorage
