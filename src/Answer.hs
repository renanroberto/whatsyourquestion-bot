{-# LANGUAGE Safe #-}

module Answer where

import SafeName (SafeName)


questionMarks :: [Char]
questionMarks = ['?', '¿', '‽', ';', '՞', '؟', '፧', '⁇', '⁈', '⁉']

isQuestion :: String -> Bool
isQuestion "" = False
isQuestion (x:xs) =
  x `elem` questionMarks && all (`elem` '!':questionMarks) xs

getAnswer :: String -> Maybe SafeName -> String
getAnswer "¿" _ = "¿ɐpᴉʌn̗p ɐns ɐ ǝ̗ ʅɐnꝹ" -- Boa noite, Bruno
getAnswer "‽" _ = "w̶h̷a̸t̶ ̸i̸s̷ ̷y̸o̷u̸r̵ ̸q̵u̶e̵s̶t̵i̴o̸n̴‽̶"
getAnswer _ Nothing = "Qual é a sua dúvida?"
getAnswer marks (Just name) =
  "Qual é a sua dúvida, " ++ show name ++ (take 10 marks)
