module Running.Tools 
( parseTimeString
) where

import Data.String.Utils
import Data.List

parseTimeString :: String -> [Float]
parseTimeString str = map read (split ':' str) :: [Float]

seconds :: [Float] -> Float
seconds [hours,mins,secs] = hours*3600 + mins*60 + secs
seconds [mins,secs] = mins*60 + secs
seconds [secs] = secs

pace :: String -> Float -> Float
pace time dist = (seconds (parseTimeString time))/dist
