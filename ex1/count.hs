import Data.Array as Array
import Control.Exception
import Debug.Trace

count n stringArray = d 2 0
 where
  d l i | i == k =trace ("l="++(show l)++" n="++(show i)) $ 1
        | (stringArray!!i) == (stringArray!!k) = mapException (addErrorInfo (" ! "++show i ++ " "++ show k))$ trace ("l="++(show l)++" i="++(show i)++" k="++(show k)++"equal") $ cps!(i,k-1)+cps!(i+1,k)+1
        | otherwise = mapException (addErrorInfo (" ! "++show i++" " ++ show k))$ trace ("l="++(show l)++" i="++(show i)++" k="++(show k)++"not equal") $ cps!(i,k-1)+cps!(i+1,k)-cps!(i+1,k-1)
        where
          k = l+i-1
  cps = Array.array ((0,0),(n+1,n+1)) [((l,i), d l i ) | (l,i) <- Array.range  ((0,0),(n+1,n+1)) ]

addErrorInfo info (ErrorCall str) = ErrorCall (str++":"++info)


countPalindromes n stringArray = dp!(0,(n-1))
 where
  d i j | i >= n || j < 0 =trace ("i="++(show i)++" j="++(show j)++" \"\" ") $ 0
        -- | dp!(i,j)  /= -1 = trace ("i="++(show i)++" j="++(show j)++" -1") $ dp!(i,j) -- give variable not be reevaluated
        | abs(i-j) == 1 = trace ("i="++(show i)++" j="++(show j)++" abs") $ if (stringArray!!i) == (stringArray!!j) then 3 else 2
        | i == j = trace ("i="++(show i)++" j="++(show j)++" ==") $ 1
        | (stringArray!!i) == (stringArray!!j) = trace ("i="++(show i)++" j="++(show j)++" stringArray") $ dp!(i+1,j) + dp!(i, j-1) +1
        | otherwise = trace ("i="++(show i)++" j="++(show j)++" other") $ dp!(i+1,j) + dp!(i, j-1) - dp!(i+1, j-1)

  dp = Array.array ((0,0),(n+1,n+1)) [((l,i), trace ("l="++(show l)++" i="++(show i)++" dp") $ d l i ) | (l,i) <- Array.range  ((0,0),(n+1,n+1)) ]
