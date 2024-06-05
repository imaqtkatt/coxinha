newtype Coxinha a = Coxinha (State a) deriving (Show)

data State a
  = Fresh a
  | Bitten a
  | Eaten
  deriving (Show)

instance Functor Coxinha where
  fmap f (Coxinha a) = Coxinha $ case a of
    Fresh x -> Bitten $ f x
    Bitten _ -> Eaten
    _ -> Eaten

instance Applicative Coxinha where
  pure = Coxinha . Fresh
  (<*>) (Coxinha f) = case f of
    Fresh x -> fmap x
    Bitten x -> \y -> case y of
      Coxinha (Bitten y') -> Coxinha $ Bitten (x y')
      y' -> fmap x y'
    Eaten -> \_ -> Coxinha Eaten

instance Monad Coxinha where
  (Coxinha a) >>= f = case a of
    Fresh x -> f x
    Bitten x -> f x
    Eaten -> Coxinha Eaten
