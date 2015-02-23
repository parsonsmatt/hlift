module Lift where

data Session = Session { lift :: LiftName 
                       , sets :: [Set] 
                       } deriving (Show)

type LiftName = String

data Set = Set { weight :: Weight
               , reps   :: Reps 
               } deriving (Show)

type Reps = Integer


sessionVolume :: Session -> Weight
sessionVolume = foldl1 (+) . map volume . sets

volume :: Set -> Weight
volume (Set wt reps) = fromInteger (reps) * wt

convert :: Weight -> Weight
convert (Wt x Lb) = Wt (x/2.2046) Kg
convert (Wt x Kg) = Wt (x*2.2046) Lb
