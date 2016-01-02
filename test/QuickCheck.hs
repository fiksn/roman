import Roman
import Test.QuickCheck
import Data.Maybe(fromMaybe)
import System.Random
import Control.Monad.State

prop_elementsHaveReverse :: Int -> Bool
prop_elementsHaveReverse n = n == actual
           where reversed = (fromRoman' . toRoman) n
                 actual = fromMaybe (error "fail") reversed 

{- Play with random roman string generation -}

randomSt :: (RandomGen g, Random a) => State g a
randomSt = state random 

randomStR :: (RandomGen g, Random a) => (a, a) -> State g a
randomStR bounds = state $ randomR bounds

numOcc :: Int -> State StdGen Int
numOcc n = do
  doGenerate <- randomSt 
  if doGenerate then 
    randomStR (1, n)
  else 
    return 0

genRandom :: [(Char, Int)] -> State StdGen String
genRandom [] = return ""
genRandom ((ch, rep):xs) = 
  do 
    num <- numOcc rep
    rest <- genRandom xs
    return $ replicate num ch ++ rest

test :: IO String
test = 
   let chars = ('-', 1) : zip (reverse romanChars) (repeat 3) in
   do 
     gen <- newStdGen
     return $ evalState (genRandom chars) gen

{- End of play -}


main = quickCheck prop_elementsHaveReverse
