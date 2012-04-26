-- |
-- Helper methods for working with formatted dates
--

module Network.TableStorage.Format where

import Data.Time
import System.Locale

getFormattedTime :: String -> IO String
getFormattedTime formatString = fmap (formatTime defaultTimeLocale formatString) getCurrentTime

rfc1123Date :: IO String
rfc1123Date = getFormattedTime rfc1123Format

atomDate :: IO String
atomDate = getFormattedTime atomDateFormat

rfc1123Format :: String
rfc1123Format = "%a, %d %b %Y %H:%M:%S GMT"

atomDateFormat :: String
atomDateFormat = "%Y-%m-%dT%H:%M:%S%QZ"