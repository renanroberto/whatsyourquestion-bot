module SafeName (SafeName, safeName, unsafeName) where

newtype SafeName = SN { unsafeName :: String }

safeName :: String -> SafeName
safeName = SN . filter (/= '@')
