module Flow (flow, (?>)) where

type FlowT a = Either a ()

class Flow a where
  flowFail :: a

  flow :: FlowT a -> a
  flow (Left x)  = x
  flow (Right _) = flowFail

  (?>) :: Bool -> a -> FlowT a
  bool ?> value = if bool then Left value else Right ()
  infix 3 ?>

instance Flow Bool where
  flowFail = False

instance Flow (Maybe a) where
  flowFail = Nothing

instance Flow e => Flow (Either e a) where
  flowFail = Left flowFail
