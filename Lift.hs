module Lift where

data Session = Session LiftName [Set]
type LiftName = String


data Set = Set { weight :: Weight
               , reps   :: Reps 
               } deriving (Show)

data Weight = Wt Float Unit
              deriving (Show, Eq)

instance Num Weight where
    Wt x Lb + Wt y Lb     = Wt (x + y) Lb
    Wt x Lb * Wt y Lb     = Wt (x * y) Lb
    negate (Wt x Lb)      = Wt (negate x) Lb
    abs (Wt x Lb)         = Wt (abs x) Lb
    signum (Wt x Lb)      = Wt (signum x) Lb
    fromInteger x         = Wt (fromInteger x) Lb

data Unit = Lb | Kg
            deriving (Show, Eq)

type Reps = Integer

volume :: Session -> Weight
volume (Session _ sets) = Wt 250 Lb

convert :: Weight -> Weight
convert (Wt x Lb) = (Wt (x/2.2) Kg)
convert (Wt x Kg) = (Wt (x*2.2) Lb)
