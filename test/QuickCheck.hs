import Roman
import Test.QuickCheck
import Data.Maybe(fromMaybe)

prop_elementsHaveReverse :: Int -> Bool
prop_elementsHaveReverse n = n == actual
           where reversed = (fromRoman' . toRoman) n
                 actual = fromMaybe (error "fail") reversed 

main = quickCheck prop_elementsHaveReverse
