module Running.Distance (
          Distance(..)
        , Foot
        , Yard
        , Mile
        , Marathon
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
    toMeters :: d -> Meter
    -- | Converts the given number of meters into the unit of measure
    fromMeters :: Meter -> d

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
    deriving (Eq, Ord, Num)

instance Distance Foot where
    toMeters   (Foot x)  = Meter(x / 3.28084) 
    fromMeters (Meter x) = Foot (x * 3.28084)

instance Show Foot where
    show (Foot x) = show x ++ "ft"

--

newtype Yard = Yard Float
    deriving (Eq, Ord, Num)

instance Distance Yard where
    toMeters   (Yard x)  = Meter(x * (1/1.09361))
    fromMeters (Meter x) = Yard (x * 1.09361)
instance Show Yard where
    show (Yard x) = show x ++ "yd"

--

newtype Mile = Mile Float
    deriving (Eq, Ord, Num)

instance Distance Mile where
    toMeters (Mile x)    = Meter(x*1609.34)
    fromMeters (Meter x) = Mile (x*(1/1609.34))

instance Show Mile where
    show (Mile x) = show x ++ "mi"

--

newtype Marathon = Marathon Float
    deriving (Eq, Ord, Num)

instance Distance Marathon where
    toMeters  (Marathon x) = Meter   (x*42194.988)
    fromMeters(Meter x)    = Marathon(x*(1/42194.988))

instance Show Marathon where
    show (Marathon x) = show x ++ "marathon"

-- | Metric Ascending

--
newtype Meter = Meter Float
    deriving (Eq, Ord, Num)

instance Distance Meter where
    toMeters x = x
    fromMeters x = x
instance Show Meter where
    show (Meter x) = show x ++ "m"
