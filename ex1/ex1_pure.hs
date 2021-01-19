import Data.Array as Array
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
-- import qualified Text.Parse.ByteString as BSP
import Data.Char (isSpace)
import Data.Array as Array
import Control.Exception
import Debug.Trace
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe
import qualified Data.List as List




-- Array.listArray (0, 5) [ x*x | x <- [0..5]]
readInt s = BSC.readInt (BSC.dropWhile isSpace s)
readInteger s = BSC.readInteger (BSC.dropWhile isSpace s)

-- readString r1 n array =

initialArray n = Array.array bounds [((i,j), initArray i j ) | (i,j) <- Array.range bounds ]
  where bounds = ((0,0),(n,n))
-- initArray:: Int->Int->Int
initArray i j
  | i == j = 1
  | otherwise = 0

main =
  do all <- BS.getContents
     -- print "hi"
     let Just (n, r1) = readInt all
     -- print n
     let (mapKeyLetter,stringMap) = convertToMap' (Map.empty,Map.empty) (n-1) ( drop 1 $ BSC.unpack r1 )
     -- print $ BSC.unpack r1
     -- print $ drop 1 $ BSC.unpack r1
     -- print stringArray
     -- print (stringArray!2)
     -- let (x, _)  = readAL r1 n Map.empty
     print stringMap
     print mapKeyLetter
     let result = countPalindromes n stringMap mapKeyLetter
     -- print $ result `mod` 20130401
     print result
     let i =2
     print (Map.lookup i stringMap)

convertToMap mp i (hd:tl)
   | i == 0 = Map.insert 0 hd mp
   | otherwise = convertToMap (Map.insert i hd mp) (i-1) tl

convertToMap' (mp',mp) i (hd:tl)
 | i == 0 = (Map.insertWith (++) hd [0] mp' ,Map.insert 0 hd mp)
 | otherwise = convertToMap' (Map.insertWith (++) hd [i] mp' ,(Map.insert i hd mp)) (i-1) tl

convertToMap'' (mp',mp) i (hd:tl)
  | i == 0 = (Map.insertWith (Set.insert) hd (Set.fromList [0]) mp' ,Map.insert 0 hd mp)
  | otherwise = convertToMap' (Map.insertWith (Set.insert) hd (Set.fromList [i]) mp' ,(Map.insert i hd mp)) (i-1) tl
-- readAL s c m =
--  do let Just (x1, s1) = readInt s
--     let Just (x2, s2) = readInt s1
--     let inewm = Map.insertWith (++) x1 [x2] m
--     let newm = Map.insertWith (++) x2 [x1] inewm
--     let (finalm, t) = readAL s2 (c-1) newm
--     if c > 0 then (finalm,t) else (m,s)

countPalindromes n stringArray mapKeyLetter  =  Map.lookup (0,(n-1)) dp
 where
  d i j | i >= n || j < 0 =trace ("i="++(show i)++" j="++(show j)++" \"\" ") $ 0
       -- | Map.lookup (i,j) dp  /= -1 = trace ("i="++(show i)++" j="++(show j)++" -1") $ Map.lookup (i,j) dp -- give variable not be reevaluated
       | abs(i-j) == 1 = trace ("i="++(show i)++" j="++(show j)++" abs") $ if List.elem j (Maybe.fromMaybe  [1,2] $ Map.lookup (Maybe.fromMaybe  'f' $ Map.lookup i stringArray) mapKeyLetter) then 3 else 2
       | i == j = trace ("i="++(show i)++" j="++(show j)++" ==") $ 1
       | List.elem j (Maybe.fromMaybe  [1,2] $ Map.lookup (Maybe.fromMaybe  'f' $ Map.lookup i stringArray) mapKeyLetter)  = trace ("i="++(show i)++" j="++(show j)++" stringArray") $ (Maybe.fromMaybe  0 $ Map.lookup (i+1,j) dp)+ (Maybe.fromMaybe 0 $ Map.lookup (i, j-1) dp) +1
       | otherwise = trace ("i="++(show i)++" j="++(show j)++" other") $ (Maybe.fromMaybe  0 $ Map.lookup (i+1,j) dp) + (Maybe.fromMaybe  0 $ Map.lookup (i, j-1) dp) - (Maybe.fromMaybe  0 $ Map.lookup (i+1, j-1) dp)
       -- | otherwise = 2

  dp = Map.fromList [((l,i), trace ("l="++(show l)++" i="++(show i)++" dp") $ d l i ) | (l,i) <- Array.range  ((0,0),(n+1,n+1)) ]
