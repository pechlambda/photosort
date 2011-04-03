module Photo (
      getTime
    ) where

import Data.Time
import System.Locale
import Graphics.Exif
import Maybe

-- бросает экзепшн, если в filePath не фотография (нет exif данных)
getTime :: String -> IO (Maybe Day)
getTime filePath =
    fromFile filePath >>= (\e -> getTag e "DateTime") >>= (return . parseDateTime)
    where
        parseDateTime (Just str) = parseTime defaultTimeLocale "%Y:%m:%d %H:%M:%S" str
        parseDateTime Nothing = Nothing
        noth _ = return Nothing
