module RunningTools (
        )
    where
import Data.Time.Clock
import Data.String.Utils
import RunningDistance

readTD :: String -> DiffTime
readTD t = secondsToDiffTime $ secs time
        where
         time = map read (split ":" t) :: [Integer]
         secs [h,m,s] = h*3600 + m*60 + s
         secs [m,s]   = m*60 + s
         secs [s]      = s

-- pace     :: DiffTime -> Distance -> String
