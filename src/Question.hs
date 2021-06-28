module Question
  ( Question
  , isQuestion
  , question
  , getQuestion
  , defaultQuestion
  , chooseAnswer
  ) where

import           Data.Function ((&))
import           SafeName      (SafeName, getName)


questionMarks :: [Char]
questionMarks = ['?', '？', '¿', '⸮', '‽', '⸘', ';', '՞', '؟', '፧', '⁇', '⁈', '⁉']

deadMarks :: [Char]
deadMarks = ['(', ')', '[', ']', '{', '}', '\'', '"', '.', '\\']

semiDeadMarks :: [Char]
semiDeadMarks = ['!']

every :: (a -> Bool) -> [a] -> Bool
every _ []    = False
every prop xs = all prop xs


newtype Question = Qst { getQuestion :: String }
  deriving (Show, Eq)

isQuestion :: String -> Bool
isQuestion msg = msg
  & filter (`notElem` allDeadMarks)
  & every (`elem` questionMarks)
  where allDeadMarks = deadMarks <> semiDeadMarks

question :: String -> Maybe Question
question str =
  if isQuestion str
     then Just . Qst . filter (`notElem` deadMarks) $ str
     else Nothing

defaultQuestion :: Question
defaultQuestion = Qst "?"


chooseAnswer :: Question -> Maybe SafeName -> String
chooseAnswer qst maybeName =
  case (getQuestion qst, maybeName) of
    ("¿" , _) -> "¿ɐpᴉʌn̗p ɐns ɐ ǝ̗ ʅɐnꝹ" -- Boa noite, Bruno
    ("⸮" , _) -> "⸮adivúd aus é lauQ"
    ("¿?", _) -> "¿Cuál es tu pregunta?"
    (";" , _) -> "Ποια είναι η ερώτησή σου;"
    ("？", _) -> "Omae wa mō shindeiru. NANI？"
    ("⸘" , _) -> "¿̴̠͗ɐ̶̧̽p̸͕̀ᴉ̵̳͛ʌ̷̘͐n̶̼̚p̷̠̍ ̴̜̔ɐ̷̟̓n̸͙͊s̴̢̀ ̸̜̽ɐ̴͍̈ ̸̱̉ǝ̷̞͋ ̶̻̈ʅ̷͙͠ɐ̶͖̐n̴͔̿Ꝺ̸̮̃"
    ("‽" , _) -> "Q̶̥̎u̸͉͒å̸͜l̷̗̀ ̷̺̈é̸̗͆ ̸̘́a̴̧̔ ̴̜̍s̶̠̃u̴͌ͅa̴̦̾ ̷͆͜d̷̪͝ú̷͊͜v̷͍̽ī̴͇d̶̖̚a̴̩͐‽̵̟̉"
    (marks, Nothing) -> "Qual é a sua dúvida" <> (take 10 marks)
    (marks, (Just name)) ->
      "Qual é a sua dúvida, " <> getName name <> (take 10 marks)
