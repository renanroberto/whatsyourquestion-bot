module Tracer where

newtype Writer w a = Writer { runWriter :: (a, w) }

instance Functor (Writer w) where
  fmap f (Writer (x, w)) = Writer (f x, w)

instance Monoid w => Applicative (Writer w) where
  pure x = Writer (x, mempty)
  Writer (f, w) <*> Writer (x, w') = Writer (f x, w <> w')

instance Monoid w => Monad (Writer w) where
  Writer (x, w) >>= f =
    let (y, w') = runWriter (f x) in Writer (y, w <> w')

tell :: w -> Writer w ()
tell w = Writer ((), w)


type Tracer = Writer [String]

trace :: String -> String -> Tracer ()
trace label msg = tell [quote label <> msg]
  where quote s = "[" <> s <> "]\t"

showTrace :: Tracer a -> IO ()
showTrace = putStr . unlines . snd . runWriter
