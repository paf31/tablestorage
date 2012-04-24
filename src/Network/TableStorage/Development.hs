-- |
-- This module contains constants for working with the storage emulator.  
--

module Network.TableStorage.Development (
  developmentAccount
) where

import Network.TableStorage.Types

developmentAccountName :: String
developmentAccountName = "devstoreaccount1"

developmentKey :: AccountKey
developmentKey = "Eby8vdM02xNOcqFlqUwJPLlmEtlCDXJ1OUzFT50uSRZ6IFsuFq2UVErCz4I6tq/K1SZFPTOtr/KBHBeksoGMGw=="

developmentHost :: String
developmentHost = "127.0.0.1"

developmentPort :: Int
developmentPort = 10002

-- |
-- An account for the storage emulator
--
developmentAccount :: Account
developmentAccount = Account { accountName = developmentAccountName,
                               accountKey = developmentKey, 
                               accountHost = developmentHost,
                               accountPort = developmentPort }