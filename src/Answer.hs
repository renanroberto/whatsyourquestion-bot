{-# LANGUAGE Safe #-}

module Answer where

import SafeName (SafeName)


isQuestion :: String -> Bool
isQuestion ('?':str) = all (`elem` ['?', '!']) str
isQuestion "¿" = True
isQuestion "‽" = True
isQuestion _ = False

getAnswer :: String -> Maybe SafeName -> String
getAnswer "¿" _ = "¿ɐpᴉʌn̗p ɐns ɐ ǝ̗ ʅɐnꝹ" -- Boa noite, Bruno
getAnswer _ Nothing = "Qual é a sua dúvida?"
getAnswer marks (Just name) =
  "Qual é a sua dúvida, " ++ show name ++ (take 10 marks)
