import Roman
import System.Environment(getArgs)
import Data.Maybe(fromMaybe)

main :: IO ()
main = do
  args <- getArgs
  putStrLn $ (fromMaybe "" . convert . head) args

