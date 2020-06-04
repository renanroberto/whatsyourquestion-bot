{-# LANGUAGE Safe #-}

module Answer where

import SafeName (SafeName)


questionMarks :: [Char]
questionMarks = ['?', '¿', '‽', '⸘', ';', '՞', '؟', '፧', '⁇', '⁈', '⁉']

isQuestion :: String -> Bool
isQuestion "" = False
isQuestion (x:xs) =
  x `elem` questionMarks && all (`elem` '!':questionMarks) xs

getAnswer :: String -> Maybe SafeName -> String
getAnswer "¿"  _ = "¿ɐpᴉʌn̗p ɐns ɐ ǝ̗ ʅɐnꝹ" -- Boa noite, Bruno
getAnswer "¿?" _ = "¿Cuál es tu pregunta?"
getAnswer ";" _ = "Ποια είναι η ερώτησή σου;"
getAnswer "‽"  _ =
  "Q̶̥̎u̸͉͒å̸͜l̷̗̀ ̷̺̈é̸̗͆ ̸̘́a̴̧̔ ̴̜̍s̶̠̃u̴͌ͅa̴̦̾ ̷͆͜d̷̪͝ú̷͊͜v̷͍̽ī̴͇d̶̖̚a̴̩͐‽̵̟̉"
getAnswer "⸘"  _ =
  "¿̴̠͗ɐ̶̧̽p̸͕̀ᴉ̵̳͛ʌ̷̘͐n̶̼̚p̷̠̍ ̴̜̔ɐ̷̟̓n̸͙͊s̴̢̀ ̸̜̽ɐ̴͍̈ ̸̱̉ǝ̷̞͋ ̶̻̈ʅ̷͙͠ɐ̶͖̐n̴͔̿Ꝺ̸̮̃"
getAnswer _ Nothing = "Qual é a sua dúvida?"
getAnswer marks (Just name) =
  "Qual é a sua dúvida, " ++ show name ++ (take 10 marks)
