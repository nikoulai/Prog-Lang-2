import Data.Array as Array
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
-- import qualified Text.Parse.ByteString as BSP
import Data.Char (isSpace)
import System.CPUTime
import Text.Printf
import Control.Monad.IO.Class (MonadIO(liftIO))
-- import Debug.Trace
import Debug.Trace



-- Array.listArray (0, 5) [ x*x | x <- [0..5]]
readInt s = BSC.readInt (BSC.dropWhile isSpace s)
readInteger s = BSC.readInteger (BSC.dropWhile isSpace s)

-- readString r1 n array =

-- initialArray n = Array.array bounds [((i,j), initArray i j ) | (i,j) <- Array.range bounds ]
--   where bounds = ((0,0),(n,n))
-- -- initArray:: Int->Int->Int
-- initArray i j
--   | i == j = 1
--   | otherwise = 0

main =
  do all <- BS.getContents
     let Just (n, r1) = readInt all
     print n
     let stringArray = Array.listArray (0,n-1) $ drop 1 $ BSC.unpack r1
     -- print $ BSC.unpack r1
     -- print $ drop 1 $ BSC.unpack r1
     -- print stringArray
     -- print (stringArray!2)
     let result = countPalindromes n stringArray
     print $ result `mod` 20130401


-- countPalindromes :: (Num e, Eq a) => Int -> [a] -> e
countPalindromes n stringArray = dp!(0,(n-1))
 where
   d i j
         -- | dp!(i,j)  /= -1 = trace ("i="++(show i)++" j="++(show j)++" -1") $ dp!(i,j) -- give variable not be reevaluated
         | abs(i-j) == 1 = trace ("i="++(show i)++" j="++(show j)++" abs") $  if (stringArray!i) == (stringArray!j) then 3 else 2
         | i == j = trace ("i="++(show i)++" j="++(show j)++" ==") $  1
         | (stringArray!i) == (stringArray!j) = trace ("i="++(show i)++" j="++(show j)++" equal") $ dp!(i+1,j) + dp!(i, j-1) +1
         | otherwise =  trace ("i="++(show i)++" j="++(show j)++" other") $ dp!(i+1,j) + dp!(i, j-1) - dp!(i+1, j-1)

   dp = Array.array ((0,0),(n,n)) [((l,i), d l i ) | (l,i) <- Array.range  ((0,0),(n,n)) ]




-- | Wrap a 'MonadIO' computation so that it prints out the execution time.
timeIt :: MonadIO m => m a -> m a
timeIt = timeItNamed "CPU time"

-- | Like 'timeIt', but uses the 'show' rendering of @a@ as label for the
-- timing.
--
-- @since 2.0
timeItShow :: (MonadIO m, Show a) => m a -> m a
timeItShow ioa = do
   (t, a) <- timeItT ioa
   liftIO $ printf (show a ++ ": %6.2fs\n") t
   return a

-- | Like 'timeIt', but uses the 'String' as label for the timing.
--
-- @since 2.0
timeItNamed :: MonadIO m => String -> m a -> m a
timeItNamed name ioa = do
   (t, a) <- timeItT ioa
   liftIO $ printf (name ++ ": %6.2fs\n") t
   return a

-- | Wrap a 'MonadIO' computation so that it returns execution time in seconds,
-- as well as the result value.
timeItT :: MonadIO m => m a -> m (Double, a)
timeItT ioa = do
   t1 <- liftIO getCPUTime
   a <- ioa
   t2 <- liftIO getCPUTime
   let t :: Double
       t = fromIntegral (t2-t1) * 1e-12
   return (t, a)
