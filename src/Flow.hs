module Flow (flow, (?>)) where

type Flow a = Either a ()

class Flowable a where
  flowFail :: a

  flow :: Flow a -> a
  flow (Left x)  = x
  flow (Right _) = flowFail

  (?>) :: Bool -> a -> Flow a
  bool ?> value = if bool then Left value else Right ()
  infix 3 ?>

instance Flowable Bool where
  flowFail = False

instance Flowable (Maybe a) where
  flowFail = Nothing

instance Flowable e => Flowable (Either e a) where
  flowFail = Left flowFail


{-- EXAMPLES
isQuestion :: String -> Bool
isQuestion msg = flow $ do
  msg == "wat"    ?> True
  length msg == 0 ?> False
  last msg == '?' ?> True

checkAnswer :: String -> Maybe String
checkAnswer msg = flow $ do
  length msg == 0   ?> Nothing
  msg == "42"       ?> Just "Correct!"
  "42" `subset` msg ?> Just "Almost Correct" 


data Error = EmptyAnswer | IsNotANumber | UnknowError
  deriving Show

instance Flowable Error where
  flowFail = UnknowError

checkAnswerWithError :: String -> Either Error String
checkAnswerWithError msg = flow $ do
  length msg == 0      ?> Left EmptyAnswer
  (not . isNumber) msg ?> Left IsNotANumber
  msg == "42"          ?> Right "Correct!"
  "42" `subset` msg    ?> Right "Almost Correct" 


subset :: Eq a => [a] -> [a] -> Bool
subset xs ys = all (\x -> x `elem` ys) xs

isNumber :: String -> Bool
isNumber = all $ \c -> [c] `elem` (show <$> digits)
  where digits :: [Int]
        digits = [0 .. 9]
--}
