module Running.Distance(
          Foot
        , Yard
        , Mile
        , Marathon
        , Meter
        , Kilometer
        )
where

-- | A generic class that defines all the units of measure in running.
-- The base unit of meters was chosen because Metric is orders more sane than
-- the English system

class Distance d where
    -- | Converts the given unit of distance into meters
    toMeters :: a -> Meter
    -- | Converts the given number of meters into the unit of measure
    fromMeters :: Meters -> a

--------------------------------------------
-- | Functions

-- | Add two distances together to get a common distance
addDist :: (Distance a, Distance b, Distance c) => a -> b -> c
addDist x y = fromMeters (toMeters x + toMeters y)

-- | Find the difference between two distcances
subDist :: (Distance a, Distance b, Distance c) => a -> b -> c
subDist x y = fromMeters (toMeters x + toMeters y)

-- | Convert one distance to another.
convertDistance :: (Distance a, Distance b) => a -> b
convertDistance = fromMeters . toMeters
