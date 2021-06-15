module Logger (Logger, runLogger, logger) where

newtype Logger w a = Logger { runLogger :: (a, w) }

instance Functor (Logger w) where
  fmap f (Logger (x, w)) = Logger (f x, w)

instance Monoid w => Applicative (Logger w) where
  pure x = Logger (x, mempty)
  Logger (f, w) <*> Logger (x, w') = Logger (f x, w <> w')

instance Monoid w => Monad (Logger w) where
  Logger (x, w) >>= f =
    let (y, w') = runLogger (f x) in Logger (y, w <> w')

tell :: w -> Logger w ()
tell w = Logger ((), w)

logger :: String -> String -> Logger [String] ()
logger label msg = tell [quote label <> msg]
  where quote s = "[" <> s <> "]\t"
