module Roman(toRoman, fromRoman, fromRoman', convert, list, romanChars) where

import Data.List
import Data.Char
import Data.Monoid
import qualified Data.Map.Strict as Map
import qualified Control.Monad as M

list = [(1000,"M"),(900,"CM"),(500,"D"),(400,"CD"),(100,"C"),(90,"XC"),(50,"L"),(40,"XL"),(10,"X"),(9,"IX"),(5,"V"),(4,"IV"),(1,"I")]

{- Invoke tests with doctest -}

-- |
-- Convert arabic number into roman numerals.
--
-- >>> toRoman 1
-- "I"
-- >>> toRoman 2
-- "II"
-- >>> toRoman 4
-- "IV"
-- >>> toRoman 5
-- "V"
-- >>> toRoman (-6)
-- "-VI"
-- >>> toRoman 2457
-- "MMCDLVII"
-- >>> toRoman 645
-- "DCXLV"
-- >>> toRoman 1234
-- "MCCXXXIV"
-- >>> toRoman 2334
-- "MMCCCXXXIV"
-- >>> toRoman 4001
-- "MMMMI"
toRoman :: Int -> String 
toRoman num = prefix ++ (concat . reverse . snd $ foldl combineToRoman (abs num, []) descList)
  where descList = sortBy descending list  {- descList due to foldl -}
        prefix = if num < 0 then "-" else ""

-- |
-- Convert roman numerals into arabic when possible.
-- Note that this function returns valid results also for "IIII", "XLX" or similar
--
-- >>> fromRoman ""
-- Just 0
-- >>> fromRoman "I"
-- Just 1
-- >>> fromRoman "iv"
-- Just 4
-- >>> fromRoman "IIII"
-- Just 4
-- >>> fromRoman "XLX"
-- Just 50
-- >>> fromRoman "LXL"
-- Just 90
-- >>> fromRoman "-VI"
-- Just (-6)
-- >>> fromRoman "GIV"
-- Nothing
-- >>> fromRoman "burek"
-- Nothing
-- >>> fromRoman "MmCdLVII"
-- Just 2457
-- >>> fromRoman "dcxlv"
-- Just 645
-- >>> fromRoman "MMMMI"
-- Just 4001
fromRoman :: String -> Maybe Int
fromRoman n = (*) <$> Just multiplier <*> M.foldM combineFromRoman 0 tokens
  where tokens = Map.toList $ Map.fromListWith (+) [(c, 1) | c <- romanTokenize input]
        input = if multiplier == 1 then map toUpper n else map toUpper $ tail n
        multiplier = if null n || (head n /= '-') then 1 else -1

-- |
-- Convert roman numerals into arabic when possible. Strict version.
--
-- >>> fromRoman' ""
-- Just 0
-- >>> fromRoman' "I"
-- Just 1
-- >>> fromRoman' "iv"
-- Just 4
-- >>> fromRoman' "IIII"
-- Nothing
-- >>> fromRoman' "XLX"
-- Nothing
-- >>> fromRoman' "LXL"
-- Nothing
-- >>> fromRoman' "-VI"
-- Just (-6)
-- >>> fromRoman' "GIV"
-- Nothing
-- >>> fromRoman' "burek"
-- Nothing
-- >>> fromRoman' "MmCdLVII"
-- Just 2457
-- >>> fromRoman' "dcxlv"
-- Just 645
-- >>> fromRoman' "MMMMI"
-- Just 4001
fromRoman' :: String -> Maybe Int
fromRoman' n 
      | input == maybe "" toRoman arabic = arabic
      | otherwise = Nothing
      where arabic = fromRoman input
            input = map toUpper n

-- |
-- Convert in both directions.
--
-- >>> convert "1999"
-- Just "MCMXCIX"
-- >>> convert "MCMXCIX"
-- Just "1999"
-- >>> convert "-MCMxCIX"
-- Just "-1999"
-- >>> convert "xix"
-- Just "19"
-- >>> convert "McM"
-- Just "1900"
-- >>> convert "FIC"
-- Nothing
-- >>> convert "XLX"
-- Nothing
-- >>> convert "MCMKIII"
-- Nothing
convert :: String -> Maybe String
convert input = getFirst $ 
           First (do                       {- first try the roman -> arabic -}
              arabic <- fromRoman' input
              Just $ show arabic) 
       `mappend` 
           First (do                       {- then also arabic -> roman -}
              num <- readMaybe input :: Maybe Int  
              Just $ toRoman num)

romanChars = reverse [toUpper . head $ s | (_, s) <- descList, length s == 1] 
  where descList = sortBy descending list

{------------------------------------------------------------------------------------------------------}
  
{- Helper functions -}

descending (a1, b1) (a2, b2) = compare a2 a1

{- [String] of the resulting accumulator is reversed -}
combineToRoman :: (Int, [String]) -> (Int, String) -> (Int, [String])
combineToRoman (accn, accs) (n, s)  =  (accn `rem` n,  replicate times s ++ accs) 
  where times =  accn `div` n    

{-------------------------------}

romanOrder :: Char -> Char -> Ordering
romanOrder a b = compare (a `elemIndex` romanChars) (b `elemIndex` romanChars)

romanTokenize :: String -> [String]
romanTokenize "" = []
romanTokenize (x:xs) = (x : fst broken) : romanTokenize (snd broken)
  where broken = break (\a -> romanOrder x a /= LT) xs

combineFromRoman :: Int -> (String, Int) -> Maybe Int
combineFromRoman acc (c, o) = do
  num <- Map.lookup c lookupMap
  Just (acc + o * num)
  where 
    lookupMap = Map.fromList [(map toUpper $ y, x) | (x, y) <- list]

readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
              [(x, "")] -> Just x
              _ -> Nothing

