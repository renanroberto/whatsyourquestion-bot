{-# LANGUAGE Safe #-}

module SafeName (SafeName, safeName) where

newtype SafeName = SN String

instance Show SafeName where
  show (SN str) = str

safeName :: String -> SafeName
safeName = SN . filter (/= '@')
