module RunningDistance (
          Distance(..)
        , Foot
        , Yard
        , Mile
        , Meter
        , Kilometer
        , addDist
        , subDist
        , convertDist
        , toFloat
        )
where

import Data.Data(Data)
import Data.List(isPrefixOf)

-- | A generic class that defines all the units of measure in running.
-- The base unit of meters was chosen because Metric is orders more sane than
-- the English system

class Distance d where
    -- | Converts the given unit of distance into meters
    toMeters   :: d -> Meter
    -- | Converts the given number of meters into the unit of measure
    fromMeters :: Meter -> d
    -- | Yields the given distance as a float
    toFloat    :: d -> Float
--------------------------------------------
-- | Functions

-- | Converts a distance to a float

--toFloat (Meter f) = f

-- | Add two distances together to get a common distance
addDist :: (Distance a, Distance b, Distance c) => a -> b -> c
addDist x y = fromMeters (toMeters x + toMeters y)

-- | Find the difference between two distcances
subDist :: (Distance a, Distance b, Distance c) => a -> b -> c
subDist x y = fromMeters (toMeters x - toMeters y)

-- | Convert one distance to another.
convertDist :: (Distance a, Distance b) => a -> b
convertDist = fromMeters . toMeters

-- | Returns weather or not a distance qualifies as an ultra marathon
-- ultraMarathon :: Distance a => a -> Bool


-- | Reads a string into a corresponding distance
readDist :: (Float -> a) -> String ->
            Int -> String ->
            [(a, String)]
readDist builder unitstr prec str = processItems builder (readsPrec prec str)
    where processItems :: (Float -> a) -> [(Float,String)] -> [(a,String)]
          processItems builder  [] = []
          processItems builder ((a,s):rest)
            | unitstr `isPrefixOf` s =
                 (builder a, drop (length unitstr) s) : (processItems builder rest)
            | otherwise              =
               processItems builder rest
--------------------------------------------
-- | Unit Definitions

-- | English, Ascending

--
newtype Foot = Foot Float
    deriving (Eq, Ord, Num, Fractional)

instance Distance Foot where
    toMeters   (Foot x)  = Meter(x / 3.28084) 
    fromMeters (Meter x) = Foot (x * 3.28084)
    toFloat    (Foot x)  = x
instance Show Foot where
    show (Foot x) = show x ++ "ft"

instance Read Foot where
    readsPrec = readDist Foot "ft"
--

newtype Yard = Yard Float
    deriving (Eq, Ord, Num, Fractional)

instance Distance Yard where
    toMeters   (Yard x)  = Meter(x * (1/1.09361))
    fromMeters (Meter x) = Yard (x * 1.09361)
    toFloat    (Yard x)  = x
instance Show Yard where
    show (Yard x) = show x ++ "yd"

instance Read Yard where
    readsPrec = readDist Yard "yd"

--
newtype Mile = Mile Float
    deriving (Eq, Ord, Num, Fractional)

instance Distance Mile where
    toMeters   (Mile x)  = Meter(x*1609.34)
    fromMeters (Meter x) = Mile (x*(1/1609.34))
    toFloat    (Mile x)  = x
instance Show Mile where
    show (Mile x) = show x ++ "mi"

instance Read Mile where
    readsPrec = readDist Mile "mi"
--

-- | Metric Ascending

--
newtype Meter = Meter Float
    deriving (Eq, Ord, Num, Fractional)

instance Distance Meter where
    toMeters   x = x
    fromMeters x = x
    toFloat (Meter x) = x
instance Show Meter where
    show (Meter x) = show x ++ "m"

instance Read Meter where
    readsPrec = readDist Meter "m"
--

newtype Kilometer = Kilometer Float
    deriving (Eq, Ord, Num, Fractional)

instance Distance Kilometer where
    toMeters (Kilometer x) = Meter    (x*1000)
    fromMeters (Meter x)   = Kilometer(x*0.001)
    toFloat (Kilometer x)  = x

instance Show Kilometer where
    show (Kilometer x) = show x ++ "km"

instance Read Kilometer where
    readsPrec = readDist Kilometer "km"
    
-- | Common Distances

--
marathon         = 26.2 :: Mile
outdoorTrack     = 400  :: Meter
indoorTrack      = 200  :: Meter
englishTrack     = 440  :: Yard
