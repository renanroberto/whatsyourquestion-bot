module SafeName (SafeName, safeName, getName) where

newtype SafeName = SN { getName :: String }
  deriving (Show, Eq)

isSafe :: String -> Bool
isSafe name = '@' `notElem` name

safeName :: String -> Maybe SafeName
safeName name =
  if isSafe name
     then Just (SN name)
     else Nothing
