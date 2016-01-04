import Roman
import Test.QuickCheck
import Data.Maybe(fromMaybe)

prop_elementsHaveReverse :: Int -> Bool
prop_elementsHaveReverse n = n == actual
           where reversed = (fromRoman' . toRoman) n
                 actual = fromMaybe (error "fail") reversed 

prop_simpleConversionShouldAlwaysSucceed :: StupidRoman -> Bool
prop_simpleConversionShouldAlwaysSucceed roman = (fromRoman $ getString roman) /= Nothing

{- Utility -}
numOcc :: Int -> Gen Int
numOcc n = do
  doGenerate <- choose (False, True) 
  if doGenerate then
    choose (1, n)
  else
    return 0

newtype StupidRoman = StupidRoman { getString :: String }
  deriving (Show, Read, Eq)

genRandom :: [(Char, Int)] -> Gen String 
genRandom [] = return ""
genRandom ((ch, rep):xs) = do 
  num <- numOcc rep
  rest <- genRandom xs
  return $ replicate num ch ++ rest

genStupid :: Int -> Gen StupidRoman
genStupid n = do 
  s <- genRandom chars
  return $ StupidRoman s
    where chars = ('-', 1) : zip (reverse romanChars) (repeat n)

instance Arbitrary StupidRoman where
  arbitrary = sized genStupid
{- Utility -}

main = do 
  quickCheck prop_elementsHaveReverse
  quickCheck prop_simpleConversionShouldAlwaysSucceed

