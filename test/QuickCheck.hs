import Roman
import Test.QuickCheck
import Data.Maybe(fromMaybe)

prop_nonZerosHaveReverse :: NonZero Int -> Bool
prop_nonZerosHaveReverse n = n == NonZero actual
           where reversed = (fromRoman' . toRoman . getNonZero) n
                 actual = fromMaybe (error "fail") reversed 

main = quickCheck prop_nonZerosHaveReverse
