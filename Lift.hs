module Lift where

data Session = Session LiftName [Set]
type LiftName = String

data Set = Set { weight :: Weight
               , reps   :: Reps 
               } deriving (Show)

type Reps = Integer

data Weight = Wt Float Unit
              deriving (Show, Eq)

instance Num Weight where
    Wt x a + Wt y b | a == b    = Wt (x + y) a
                    | otherwise = Wt x a + convert (Wt y b)
    Wt x a * Wt y b | a == b    = Wt (x * y) a
                    | otherwise = Wt x a * convert (Wt y b)
    negate (Wt x a) = Wt (negate x) a
    signum (Wt x a) = Wt (signum x) a
    fromInteger x   = Wt (fromInteger x) Lb
    abs (Wt x a)    = Wt (abs x) a

instance Fractional Weight where
    fromRational a  = Wt (fromRational a) Lb
    Wt x a / Wt y b | a == b    = Wt (x / y) a
                    | otherwise = Wt x a / convert (Wt y b)

data Unit = Lb | Kg
            deriving (Show, Eq)

volume :: Session -> Weight
volume (Session _ sets) = Wt 250 Lb

predict1RM :: Weight -> Reps -> Weight
predict1RM w r = w + w * (fromInteger r) * 0.0333

convert :: Weight -> Weight
convert (Wt x Lb) = (Wt (x/2.2046) Kg)
convert (Wt x Kg) = (Wt (x*2.2046) Lb)
