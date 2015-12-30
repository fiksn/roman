import Roman
import Test.QuickCheck
import Data.Maybe(fromMaybe)

prop_positivesHaveReverse :: Positive Int -> Bool
prop_positivesHaveReverse n = n == Positive actual
           where reversed = (fromRoman' . toRoman . getPositive) n
                 actual = fromMaybe (error "fail") reversed 

main = quickCheck prop_positivesHaveReverse
