-- |
-- This module contains constants for working with the storage emulator.
--

module Network.TableStorage.Development (
  developmentAccount, developmentConf
) where

import Network.TableStorage.Types
    ( Account(..), AccountKey(AccountKey), TableConf(..) )

-- |
-- An account for the storage emulator
--
developmentAccount :: Account
developmentAccount = Account { accountScheme            = "http:",
                               accountHost              = "127.0.0.1" ,
                               accountName              = "devstoreaccount1",
                               accountPort              = 10002,
                               accountResourcePrefix    = "/devstoreaccount1",
                               accountKey               = AccountKey "Eby8vdM02xNOcqFlqUwJPLlmEtlCDXJ1OUzFT50uSRZ6IFsuFq2UVErCz4I6tq/K1SZFPTOtr/KBHBeksoGMGw=="}

developmentConf :: TableConf
developmentConf = TableConf developmentAccount Nothing Nothing