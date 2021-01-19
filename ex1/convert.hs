import qualified Data.Map as Map
import qualified Data.Set as Set

convertToMap' (mp',mp) i (hd:tl)
  | i == 0 = (Map.insertWith (++) hd [0] mp' ,Map.insert 0 hd mp)
  | otherwise = convertToMap' (Map.insertWith (++) hd [i-1] mp' ,(Map.insert i hd mp)) (i-1) tl
