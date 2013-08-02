module Running.Distance (
          Distance(..)
        , Foot
        , Meter
        , addDist
        , subDist
        , convertDist
        )
where

import Data.Data(Data)
import Data.List(isPrefixOf)
import Data.Typeable(Typeable)

-- | A generic class that defines all the units of measure in running.
-- The base unit of meters was chosen because Metric is orders more sane than
-- the English system

class Distance d where
    -- | Converts the given unit of distance into meters
    toMeters :: d -> Float
    -- | Converts the given number of meters into the unit of measure
    fromMeters :: Float -> d

--------------------------------------------
-- | Functions

-- | Add two distances together to get a common distance
addDist :: (Distance a, Distance b, Distance c) => a -> b -> c
addDist x y = fromMeters (toMeters x + toMeters y)

-- | Find the difference between two distcances
subDist :: (Distance a, Distance b, Distance c) => a -> b -> c
subDist x y = fromMeters (toMeters x + toMeters y)

-- | Convert one distance to another.
convertDist :: (Distance a, Distance b) => a -> b
convertDist = fromMeters . toMeters

--------------------------------------------
-- | Unit Definitions

-- | English, Ascending

--
newtype Foot = Foot Float
    deriving (Eq, Data, Num,Ord,Real,Typeable)

instance Distance Foot where
    toMeters (Foot x) = x*0.3048
    fromMeters x      = Foot (x * 3.28084)

instance Show Foot where
    show (Foot x) = show x ++ "ft"


-- | Metric Ascending

--
newtype Meter = Meter Float
    deriving (Eq, Data, Num,Ord,Real,Typeable)

instance Distance Meter where
    toMeters (Meter x) = x
    fromMeters x       = x
instance Show Meter where
    show (Meter x) = show x ++ "m"
